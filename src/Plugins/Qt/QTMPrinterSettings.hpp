
/******************************************************************************
 * MODULE     : QTMPrinterSettings.hpp
 * DESCRIPTION: 
 * COPYRIGHT  : (C) 2010 Miguel de Benito Delgado
 *******************************************************************************
 * This software falls under the GNU general public license version 3 or later.
 * It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
 * in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
 ******************************************************************************/

#ifndef QTMPRINTERSETTINGS_HPP
#define QTMPRINTERSETTINGS_HPP

#include <QObject>

#include <QPrinter>  // Provides QPrinter::PaperSize
#include <QProcess>
#include <QHash>

/* Mock Interface of the Asynchronous command stuff
class QTMAsynchronousCommand {
public:
  QTMAsynchronousCommand();
  
  virtual bool slotStart() = 0;
  virtual void signalDone() = 0;
protected:
  virtual void signalFinished() = 0;
};
*/
 
/*!
 * This class holds the printing options that we are able to manage or
 * heed, as well as list of available ones relevant to those. For instance, 
 * it can query the printing system as to the available paper sizes for a given 
 * printer. 
 * 
 * This class provides one signal, doneReading(), which will be emmitted after the
 * system command used to read the configuration variables finishes. Why bother?
 * Because lpoptions uses cupsGetDests() from the CUPS API, which in turn opens
 * a connection to the server (using cupsGetDests2(), see cups/dest.c in CUPS
 * source code, v1.4.5). If it takes too long to reply, we are going to freeze
 * the interface.
 */
class QTMPrinterSettings : public QObject {
  Q_OBJECT
  
public:
  //! Left-Right_Top-Bottom, Right-Left_Top-Bottom, etc.
  enum PagePrintingOrder { LR_TB, RL_TB, TB_LR, TB_RL,
                           LR_BT, RL_BT, BT_LR, BT_RL};
  
  //! Arguments to the "-o orientation-requested=" CUPS option
  enum PageOrientation { Portrait        = 3, Landscape        = 4,
                         ReversePortrait = 5, ReverseLandscape = 6 }; 
  
  /*! The different settings, for which we can read available choices from the 
   * driver. See getChoices(). */
  enum DriverChoices { PageSize, Resolution, Duplex, ColorModel, Collate };
  
  //! TODO: Use this.
  enum ColorMode { Monochrome, Gray8Bit, Color};
  
  bool   collateCopies;
  bool      blackWhite;  //! Force black & white printing even on color printers
  QString  printerName;
  QString     fileName;
  QString    paperSize;
  QString printProgram;  
  int              dpi;
  int        firstPage;  //! If firstPage == lastPage == 0, print everything
  int         lastPage;
  bool   printOddPages;
  bool  printEvenPages;
  bool       fitToPage;
  int        copyCount;
  bool          duplex;  //! Print on both sides of the paper
  int     pagesPerSide;  //! Can be one of 1,2,4,6,9,16
  PagePrintingOrder pagesOrder;
  PageOrientation  orientation;
  
public:
  QTMPrinterSettings();
  
  void getFromQPrinter(const QPrinter&);
  void setToQPrinter(QPrinter&) const;
  
  QStringList getChoices(DriverChoices _which, int& _default);
  
  /*! Implemented by one of CupsQTMPrinterSettings, WinQTMPrinterSettings */
  virtual QString toSystemCommand() const = 0;
  
  /*! 
   *  Implemented by one of CupsQTMPrinterSettings, WinQTMPrinterSettings 
   *  Must return a list of pairs of strings. The first item in each pair being
   *  the printer's display name (i.e. the one to be shown to the user), the
   *  second the queue name.
   */
  virtual QList<QPair<QString,QString> > availablePrinters() = 0;
  
  static QString qtPaperSizeToQString(const QPrinter::PaperSize);
  static QPrinter::PaperSize qStringToQtPaperSize(const QString&);

signals:
  void doneReading();

public slots:
  void startReadingSystemConfig(const QString& pname) { fromSystemConfig(pname); }

protected slots:
  virtual void systemCommandFinished(int exitCode,
                                     QProcess::ExitStatus exitStatus) = 0;
  
protected:
  /*! Implemented by one of CupsQTMPrinterSettings, WinQTMPrinterSettings */
  virtual bool fromSystemConfig(const QString& printer) = 0;
  
  QProcess* configProgram;
  QHash<QString, QString> printerOptions;
};

#if defined(Q_OS_MAC) || defined (Q_OS_LINUX)

/*!
 * Implementation specific to the CUPS driver under MacOS and Linux. This
 * basically means we assume both "lp" and "lpoptions" are installed in the
 * system and accept the standard arguments. See for instance:
 * @link http://www.cups.org/documentation.php/options.html @endlink
 */
class CupsQTMPrinterSettings : public QTMPrinterSettings {
  // Q_OBJECT
  // MOC does not support conditional compilation    
public:
  CupsQTMPrinterSettings();
  QString toSystemCommand() const;
  QList<QPair<QString,QString> > availablePrinters();
protected:
  bool fromSystemConfig(const QString& printer);
  void systemCommandFinished(int exitCode, QProcess::ExitStatus exitStatus);
  };

#endif  // defined(Q_WS_MAC) || defined (Q_WS_X11)

#ifdef Q_OS_WIN

/*!
 * A WinQTMPrinterSettings object sets the the default print command to be either
 * Ghostscript's mswinpr2 driver or the tool gsprint.
 *
 * The GPL Ghostscript docs state
 * that "The mswinpr2 device uses MS Windows printer drivers, and thus should 
 * work with any printer with device-independent bitmap (DIB) raster
 * capabilities". See:
 * @link http://pages.cs.wisc.edu/~ghost/doc/svn/Devices.htm#Win @endlink
 *
 * However, the author of that driver has written another tool which should
 * perform better on colour printers. See 
 * @link http://pages.cs.wisc.edu/~ghost/gsview/gsprint.htm @endlink
 
 */
class WinQTMPrinterSettings : public QTMPrinterSettings {
  // Q_OBJECT
  // MOC does not support conditional compilation  
public:
  WinQTMPrinterSettings();
  QString toSystemCommand() const;
  QList<QPair<QString,QString> > availablePrinters();
protected:
  bool fromSystemConfig(const QString& printer);
  void systemCommandFinished(int exitCode, QProcess::ExitStatus exitStatus);
};

#endif // Q_WS_WIN

#endif  // QTMPRINTERSETTINGS_HPP
