#ifndef QTMMacPasteboardMimePDF_HPP
#define QTMMacPasteboardMimePDF_HPP

#if defined(Q_OS_MAC) && QT_VERSION < 0x060000 

#include <QMacPasteboardMime>
#include <QString>
#include <QList>
#include <QByteArray>
#include <QVariant>

// On MacOS we have to register appropriate mime types for PDF files
// The QMacPasteboardMimePDF class is instantiated in QTMApplication
// and provides the necessary support.
//
// code from:
// https://www.lyx.org/trac/browser/lyxsvn/lyx-devel/trunk/src/frontends/qt4/GuiApplication.cpp?rev=24894

// (mg) I'm not sure this is the right place to have this code, but well...

class QMacPasteboardMimePDF : public QMacPasteboardMime
{
public:
  QMacPasteboardMimePDF ()
    : QMacPasteboardMime (MIME_QT_CONVERTOR | MIME_ALL)
  {}

  QString convertorName() { return "PDF"; }

  QString flavorFor (QString const & mime)
  {
    if (mime == QLatin1String ("application/pdf"))
      return QLatin1String ("com.adobe.pdf");
    return QString();
  }

  QString mimeFor(QString flav)
  {
    if (flav == QLatin1String ("com.adobe.pdf"))
      return QLatin1String ("application/pdf");
    return QString ();
  }

  bool canConvert(QString const & mime, QString flav)
  { return mimeFor (flav) == mime; }

  QVariant convertToMime (QString const & mime, QList<QByteArray> data, QString flav)
  {
    (void) flav; (void) mime;
    if (data.count () > 1)
      debug_qt << "QMacPasteboardMimePDF: Cannot handle multiple member data " << LF;
    return data.first ();
  }

  QList<QByteArray> convertFromMime (QString const & mime, QVariant data, QString flav)
  {
    (void) flav; (void) mime;
    QList<QByteArray> ret;
    ret.append (data.toByteArray ());
    return ret;
  }
};
#endif // defined(Q_OS_MAC) && QT_VERSION < 0x060000 

#endif // defined QTMMacPasteboardMimePDF_HPP