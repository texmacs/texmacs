
/******************************************************************************
* MODULE     : WINPrint.cpp
* DESCRIPTION: printing for windows using poppler and Qt
* COPYRIGHT  : (C) 2013  Denis Raux
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#if defined (QTTEXMACS) && (defined (__MINGW__) || defined (__MINGW32__))
#include <QtGlobal>
#include <QtCore/Qstring>
#if (QT_VERSION >= 0x050000)
#include <QtWidgets>
#else
#include <QtGui>
#endif
#include "poppler/qt4/poppler-qt4.h"
#include "WINPrint.hpp"
#include "tm_ostream.hpp"

QPrinter* WINPrint::Prt=NULL;

WINPrint::WINPrint (QString url, bool &IsLandscape) {
  if (Prt == NULL) Prt= new QPrinter(QPrinter::HighResolution);
  if (IsLandscape == true) Prt->setOrientation (QPrinter::Landscape);
  else Prt->setOrientation (QPrinter::Portrait);
  QPrintDialog Pdlg (Prt);
  if (Pdlg.exec() == QDialog::Accepted && !url.isNull() && !url.isEmpty()) {
    doit= true;  
    file= url;
    first_page= Prt->fromPage();
    last_page = Prt->toPage();
    if (first_page + last_page == 0) {
      first_page= 1;
      last_page = 1000000;
    }
  }
  else doit= false;
}

WINPrint::~WINPrint() {
  if (!doit) return;
  Poppler::Document* document = Poppler::Document::load(file);
  if (document) {
    document->setRenderHint(Poppler::Document::Antialiasing);
    document->setRenderHint(Poppler::Document::TextAntialiasing);

    int nbpages=document->numPages(), nextpage=nbpages-1;
    QPainter Paint;
    if(Paint.begin(Prt)) {
      QImage image;
      QRect rect (0, 0,
                  Prt->paperRect (QPrinter::DevicePixel).width(),
                  Prt->paperRect(QPrinter::DevicePixel).height());
      double rres= Prt->resolution();
      Paint.setRenderHint (QPainter::Antialiasing);
      for (int pg=0;pg < nbpages;pg++) {
        Poppler::Page* pdfPage = document->page (pg);  
        if (pdfPage) {
          image= pdfPage->renderToImage (rres, rres);
          delete pdfPage;
        }
        if (!image.isNull()) {
          Paint.drawImage (rect, image);
          if (pg != nextpage) Prt->newPage();
        }
        else convert_error << "Fail to create image at "
                           << rres << " dpi resolution\n";
      }
      Paint.end();
    }
    delete (document);
  }
}

#endif
