
/******************************************************************************
 * MODULE     : QTMPrinterSettings.cpp
 * DESCRIPTION:
 * COPYRIGHT  : (C) 2010 Miguel de Benito Delgado
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#include "QTMPrinterSettings.hpp"
#include "qt_utilities.hpp"
#include "sys_utils.hpp"

#include <QPrinter>
#include <QPrinterInfo>
#include <QProcess>
#include <QRegExp>

/*!
 *
 */
QTMPrinterSettings::QTMPrinterSettings()
: collateCopies(true), blackWhite(true), printerName(""), fileName(""), 
  paperSize("A4"), dpi(600), firstPage(0), lastPage(0), printOddPages(true), 
  printEvenPages(true), fitToPage(true), copyCount(1), duplex(false),
  pagesPerSide(1), pagesOrder(LR_TB), orientation(Portrait)
{
  configProgram = new QProcess(this);

  // See the class documentation for an explanation of the connected signal
  QObject::connect(configProgram, SIGNAL(finished(int, QProcess::ExitStatus)),
                   this, SLOT(systemCommandFinished(int, QProcess::ExitStatus)));
}

/*!
 * Reads the configuration parameters from a QPrinter object.
 * @todo Check this sets all the available parameters.
 */
void
QTMPrinterSettings::getFromQPrinter(const QPrinter& from) {
  printerName   = from.printerName ();
  fileName      = from.outputFileName ();
  orientation   = (from.orientation() == QPrinter::Landscape) 
                  ? Landscape : Portrait;
  paperSize     = qtPaperSizeToQString(from.paperSize());
  dpi           = from.resolution ();
  firstPage     = from.fromPage ();
  lastPage      = from.toPage ();
#if (QT_VERSION >= 0x040700)
  copyCount     = from.copyCount ();
#endif
  collateCopies = from.collateCopies();
  blackWhite    = (from.colorMode () == QPrinter::Color);
  printProgram  = from.printProgram();
}

/*!
 * Sets the configuration parameters to a QPrinter object.
 * @todo Check this sets all the available parameters.
 */
void
QTMPrinterSettings::setToQPrinter(QPrinter& to) const {
  to.setResolution(dpi);
  to.setFromTo(firstPage, lastPage);  
  to.setOrientation((orientation == Landscape) ?
                    QPrinter::Landscape : QPrinter::Portrait);
  to.setOutputFileName(fileName);
  to.setPaperSize(qStringToQtPaperSize(paperSize));
#if (QT_VERSION >= 0x040700)
  to.setCopyCount(copyCount);
#endif  
  to.setCollateCopies(collateCopies);
  to.setColorMode(blackWhite ? QPrinter::Color : QPrinter::GrayScale);
}

/*!
 * Just for internal use, converts QPrinter::PaperSize to a string 
 * representation. Massimiliano's code.
 */
QString
QTMPrinterSettings::qtPaperSizeToQString(const QPrinter::PaperSize _size) {
  
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

/*!
 * Just for internal use, converts a string to QPrinter::PaperSize.
 */
QPrinter::PaperSize
QTMPrinterSettings::qStringToQtPaperSize(const QString& _size) {
  
#define PAPER(fmt)  if(_size == "fmt") return QPrinter::fmt
  PAPER (A0) ; PAPER (A1) ; PAPER (A2) ; PAPER (A3) ; PAPER (A4) ;
  PAPER (A5) ; PAPER (A6) ; PAPER (A7) ; PAPER (A8) ; PAPER (A9) ;
  PAPER (B0) ; PAPER (B1) ; PAPER (B2) ; PAPER (B3) ; PAPER (B4) ;
  PAPER (B5) ; PAPER (B6) ; PAPER (B7) ; PAPER (B8) ; PAPER (B9) ;
  PAPER (B10) ;  PAPER (Letter) ;
  return QPrinter::A4;  // Default
#undef PAPER
}


/*!
 * Returns a QStringList with all the available choices reported by the
 * printer driver for the setting specified. 
 *
 * @param _which Specifies which driver option to query.
 * @param _default Will hold the index number (in the returned list) of the 
 *        currently selected value, as reported by the printer driver.
 */
QStringList
QTMPrinterSettings::getChoices(DriverChoices _which, int& _default) {
  QStringList _ret;
  switch (_which) {
    case PageSize:
      _ret = printerOptions["PageSize"].split(" ", QString::SkipEmptyParts);
      break;
    case Resolution:
      _ret = printerOptions["Resolution"].split(" ", QString::SkipEmptyParts);
      break;
    case Duplex:
      _ret = printerOptions["Duplex"].split(" ", QString::SkipEmptyParts);
      break;
    case ColorModel:
      _ret = printerOptions["ColorModel"].split(" ", QString::SkipEmptyParts);
      break;
    case Collate:
      _ret = printerOptions["Collate"].split(" ", QString::SkipEmptyParts);
      break;
  }
  
  // FIXME: this is CUPS specific (marking the default option with an asterisk)
  for (int i=0; i<_ret.size(); ++i)
    if (_ret[i].trimmed().startsWith("*")) {
      _ret[i] = _ret[i].trimmed().remove(0,1);
      _default = i;
    }
  return _ret;
}


#if defined(Q_WS_MAC) || defined(Q_WS_X11)

/*!
 * @todo Read the printing program from some configuration file/preference?
 */
CupsQTMPrinterSettings::CupsQTMPrinterSettings() : QTMPrinterSettings() {
  printProgram = "lp ";
}

/*!
 * Runs the lpoptions program and returns. 
 */
bool
CupsQTMPrinterSettings::fromSystemConfig(const QString& printer) {
  if (configProgram->state() != QProcess::NotRunning)
    return false;
  
  // Watch out! the order of the options is relevant!
  configProgram->start(QString("lpoptions -p \"%1\" -l").arg(printer));
  //, 
  //                   QStringList() << QString("-p \"%1\"").arg(printer)
  //                                 << "-l");
  
  
  printerName = printer;
  return true;
}

/*!
 * Parses the output of the lpoptions program once it finishes. This 
 * asynchronous calling of lpoptions is needed because it performs network
 * calls. The available configuration choices are available as QStringLists
 * via QTMQTMPrinterSettings::getChoices().
 *
 * @warning This depends on the output format and follows the implementation
 *          found on CUPS 1.4.5 (systemv/lpoptions.c) A sample of the output
 *          follows:
 * Resolution/Printer Resolution: *3600x3600dpi 1200x600dpi
 * PageSize/Page Size: Letter Legal Executive HalfLetter 4x6 5x7 5x8 *A4 A5 A6 
 * InputSlot/Paper Source: Auto Tray1 Tray2 Tray3 Tray1_Man
 * Duplex/2-Sided Printing: *None DuplexNoTumble DuplexTumble
 * Collate/Collate: True *False
 * ColorModel/Print Color as Gray: Gray *CMYK
 */
void
CupsQTMPrinterSettings::systemCommandFinished(int exitCode, 
                                              QProcess::ExitStatus exitStatus) {
  (void) exitCode;
  
  printerOptions.clear();

  if (exitStatus != QProcess::NormalExit) {
    emit doneReading();
    return;
  }
    
  QRegExp rx("^(\\w+)/(.+):(.*)$"); // Param/Param desc: val1 val2 *default val4
  rx.setMinimal(true);              // Non-greedy matching
  
  QList<QByteArray> _lines = configProgram->readAllStandardOutput().split('\n');
  foreach (QString _line, _lines) {
    if(rx.indexIn(_line) == -1)      // No matches?
      continue;
    // Store for further parsing later, see QTMPrinterSettings::getChoices()
    printerOptions[rx.cap(1)] = rx.cap(3);   
  }
  emit doneReading();
}


/*!
 * Returns a command string to be run on the shell. It is assumed that CUPS is
 * available in the machine. Please see
 * @link http://www.cups.org/documentation.php/options.html @endlink
 * for the list of all CUPS options.
 */
QString
CupsQTMPrinterSettings::toSystemCommand() const {
  QString _cmd;

  if (from_qstring (printProgram) == "lp ")
    _cmd += to_qstring (get_printing_cmd ()) + " ";
  
  else if (! printProgram.isEmpty())
    _cmd += printProgram;
  
  if (! printerName.isEmpty())
    _cmd += QString(" -d \"%1\"").arg(printerName);
  
  _cmd += QString(" -o orientation-requested=%1").arg(orientation);
  
  if (duplex && (orientation==Landscape || orientation == ReverseLandscape))
    _cmd += " -o sides=two-sided-short-edge";
  else if (duplex && (orientation==Portrait || orientation == ReversePortrait))
    _cmd += " -o sides=two-sided-long-edge";
 
  if (fitToPage)
    _cmd += " -o fitplot";

  if (pagesPerSide > 1) {
    _cmd += QString(" -o number-up=%1").arg(pagesPerSide);

    // -o number-up-layout=string
    // specifies the n-up image order in any of eight permutations from btlr 
    // (bottom, top, left, right) to tbrl.
    _cmd += " -o number-up-layout=";
    switch (pagesOrder) {
      case LR_TB: _cmd += "lrtb"; break; case RL_TB: _cmd += "rltb"; break;
      case TB_LR: _cmd += "tblr"; break; case TB_RL: _cmd += "tbrl"; break;
      case LR_BT: _cmd += "lrbt"; break; case RL_BT: _cmd += "rlbt"; break;
      case BT_LR: _cmd += "btlr"; break; case BT_RL: _cmd += "btrl"; break;
      default: _cmd += "lrtb"; break;
    }
  }
  
  // Specifies which pages to print in the document. The list can contain a list
  // of numbers and ranges (#-#) separated by commas (e.g. 1,3-5,16).
  // The page numbers refer to the output pages and not the document's original
  // pages - options like "number-up" can affect the numbering of the pages.
  if (firstPage != 0 || lastPage != 0) {
    int f = (int)floor(firstPage / pagesPerSide);    f = (f==0) ? 1 : f;
    int l = (int)ceil (lastPage / pagesPerSide);     l = (l==0) ? 1 : l;
    if (firstPage > lastPage )
      _cmd += " -o outputorder=reverse";
    
    _cmd += QString(" -o page-ranges=%1-%2").arg(f).arg(l);
  }
    
  if (copyCount > 1) {
    _cmd += QString(" -o Collate=") + (collateCopies ? "True" : "False");
    _cmd += QString(" -n %1").arg(copyCount);
  }
  
  if (! printOddPages)
    _cmd += " -o page-set=even";
  else if(! printEvenPages)
    _cmd += " -o page-set=odd";

  _cmd += " -- ";  // Marks the end of options; use this to print a file whose 
                   // name begins with a dash (-).
  _cmd += '"' + fileName + '"';
  
  return _cmd;
}


/*!
 * This is a temporary workaround the problem with the names returned for the
 * printers by QPrinterInfo::availablePrinters().
 * @see QTMPrinterSettings::availablePrinters()
 * @todo Use asynchronous querying.
 */
QList<QPair<QString,QString> >
CupsQTMPrinterSettings::availablePrinters() {
  QList<QPair<QString,QString> > _ret;
  QProcess stat(this);
  stat.start("lpstat -a");
  if(! stat.waitForFinished(2000)) // 2 sec.
    return _ret;
  QRegExp rx("^(\\S+) +.*$");
  rx.setMinimal(true);
  QList<QByteArray> _lines = stat.readAllStandardOutput().split('\n');
  foreach (QString _line, _lines) {
    if(rx.indexIn(_line) == -1)      // No matches?
      continue;
    _ret << QPair<QString,QString>(rx.cap(1),rx.cap(1));
  }
  return _ret;
}

#endif

#ifdef Q_WS_WIN 

/*!
 * 
 * @todo Read the printing program from some configuration file/preference?
 */
WinQTMPrinterSettings::WinQTMPrinterSettings() : QTMPrinterSettings() {
  // -sDEVICE=mswinpr2
  // Selects the MS Windows printer device.
  //  
  // -dNoCancel
  // Hides the progress dialog, which shows the percent of the document page 
  // already processed and also provides a cancel button.
  //printProgram = "gs -sDEVICE=mswinpr2";
  printProgram = "gsprint";
}

/*!
 * Uses Winprinfo ( @link http://unixwiz.net/tools/winprinfo.html @link ) to
 * read configuration parameters from the printer.
 */
bool 
WinQTMPrinterSettings::fromSystemConfig(const QString& printer) {
  if (configProgram->state() != QProcess::NotRunning)
    return false;
  /* This (untested) alternative uses (wrong) postscript to read printer options
  configProgram->start(QString("gs"), QStringList() 
                       << "-sDEVICE=mswinpr2"
                       << QString("-sOutputFile=\"\\\\spool\\%1\"").arg(printer)
                       << "-c \"currentpagedevice /Duplex get ==\"");
   */
  
  // See the docs for QProcess::start() for the reason behind the triple \"
  configProgram->start(QString("winprinfo --printer=\"\"\"%1\"\"\"").arg(printer));
  printerName = printer;
  return true;
}

/*!
 * Uses either gs with the mswinpr2 driver or gsprint.
 * @see WinQTMPrinterSettings
 * @fixme Completely untested.
 * @todo Use enum ColorMode with blackWhite or something like that to force
 *        monochrome/grey
 */
QString
WinQTMPrinterSettings::toSystemCommand() const {
  QString _cmd;
  
  if (! printProgram.isEmpty())
    _cmd += printProgram;
  
  /* 
  // This (untested) code is for use with the mswinpr2 driver:
  _cmd += QString(" -sOutputFile=\"\\\\spool\\%1\" ").arg(printerName);
  _cmd += QString(" -c << /Duplex %1 /Tumble %2 >> setpagedevice").
              arg(duplex ? "true" : "false").
              arg((orientation == Landscape || orientation == ReverseLandscape) 
                  ? "true" : "false");
  
  // -f Interprets following non-switch arguments as file names to be executed
  // using the normal run command. Since this is the default behavior, 
  // -f is useful only for terminating the list of tokens for the -c switch.
  _cmd += QString(" -f \"%1\"").arg(fileName);
  */
  
  _cmd += " -noquery";  // Don't show printer setup dialog.

  if (! printerName.isEmpty())
    _cmd += QString(" -printer \"%1\"").arg(printerName);
  
  if (duplex)
    _cmd += QString(" -duplex_%1").
                arg((orientation == Portrait || orientation == ReversePortrait)
                    ? "vertical" : "horizontal");
  else
    _cmd += (orientation == Portrait || orientation == ReversePortrait)
             ? " -portrait" : " -landscape";    
  
  if (pagesPerSide > 1)
    _cmd += QString(" -copies %1").arg(pagesPerSide);
    
  // Specifies which pages to print in the document. The list can contain a list
  // of numbers and ranges (#-#) separated by commas (e.g. 1,3-5,16).
  // The page numbers refer to the output pages and not the document's original
  // pages - options like "number-up" can affect the numbering of the pages.
  if (firstPage != 0 || lastPage != 0) {
    int f = (int)floor(firstPage / pagesPerSide);    f = (f==0) ? 1 : f;
    int l = (int)ceil (lastPage / pagesPerSide);     l = (l==0) ? 1 : l;
    // FIXME: what happens if f > l?
    _cmd += QString(" -from %1 -to %2").arg(f).arg(l);
  }
  
  if (copyCount > 1)
     _cmd += QString(" -copies %1").arg(pagesPerSide);
  
  if (! printOddPages)
    _cmd += " -even";
  else if(! printEvenPages)
    _cmd += " -odd";
  
  _cmd += '"' + fileName + '"';
  
  return _cmd;
}

/*!
 * Parses winprinfo output. This is really UGLY.
 */
void
WinQTMPrinterSettings::systemCommandFinished(int exitCode, 
                                           QProcess::ExitStatus exitStatus) {
  (void) exitCode;
  
  printerOptions.clear();
  
  if (exitStatus != QProcess::NormalExit) {
    emit doneReading();
    return;
  }
  
  int resolutionsCounter = 0;
  bool readingSizes = false;
  QList<QByteArray> _lines = configProgram->readAllStandardOutput().split('\n');
  foreach (QString _line, _lines) {
    // Parse special lines after the DC_ENUMRESOLUTIONS : (see below)
    if (resolutionsCounter > 0) {
      --resolutionsCounter;
      QRegExp rx2("^.*x=(\\d)+.*y=(\\d)+.*$");
      rx2.setMinimal(true);
      if (rx2.indexIn(_line) > -1)
        printerOptions["Resolution"] += QString("%1x%2dpi ").
                                                arg(rx2.cap(1)).arg(rx2.cap(2));
      continue;
    }

    if (_line.contains("PAPER SIZES FROM THE DEVMODE")) {
      readingSizes = true;
      //printerOptions["PaperSize"] = QString();
      continue;
    }
                                                    
    // Parse special lines after PAPER SIZES FROM THE DEVMODE :   
    if (readingSizes) {
      QRegExp rx2("^.*mm *(\\w)+.*$");  // [ 0]   215.90  279.40 mm  Letter
      rx2.setMinimal(true);
      if (rx2.indexIn(_line) > -1)
        printerOptions["PaperSize"] += rx2.cap(1);
      continue;
    }

    QRegExp rx("^ *DC_(\\w+) *(\\w+) *$"); // DC_SOMETHING    <num>
    rx.setMinimal(true);                   // Non-greedy matching
    if(rx.indexIn(_line) == -1)      // No matches?
      continue;
    QStringList capt = rx.capturedTexts();
    if (capt.size() != 3)              // We are only interested in some options.
      continue;
    
    if (capt[1] == "DUPLEX" && capt[2].toInt() > 0)
      printerOptions["Duplex"] = "Yes No"; 
    if (capt[1] == "COLORDEVICE" && capt[2].toInt() > 0)
      printerOptions["ColorModel"] = "Monochrome Gray Color";
    if (capt[1] == "COLLATE") {
      if (capt[2].toInt() > 0) {
        printerOptions["Collate"] = "No *Yes";
      } else {
        printerOptions["Collate"] = "*No Yes";
      }
    }
    if (capt[1] == "ENUMRESOLUTIONS") {
      resolutionsCounter = capt[2].toInt();   // The next iterations are special
      //printerOptions["Resolution"] = QString();
    }
  }
  
  emit doneReading();
}

/*!
 * @see QTMPrinterSettings::availablePrinters()
 * @todo Check whether the list returned by QPrinterInfo is ok.
 */
QList<QPair<QString,QString> >
WinQTMPrinterSettings::availablePrinters() {
  QList<QPair<QString,QString> > _ret;
  foreach(QPrinterInfo printer, QPrinterInfo::availablePrinters())
    _ret << QPair<QString,QString>(printer.printerName(), printer.printerName());
  return _ret;
}

#endif

