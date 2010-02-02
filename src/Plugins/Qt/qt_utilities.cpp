
/******************************************************************************
* MODULE     : qt_utilities.mm
* DESCRIPTION: Utilities for QT
* COPYRIGHT  : (C) 2007  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "qt_utilities.hpp"
#include <QImage>
#include <QCoreApplication>
#include "dictionary.hpp"
#include "converter.hpp"


QRect
to_qrect (const coord4 & p) {
  float c= 1.0/PIXEL;
  return QRect (p.x1*c, -p.x4*c, (p.x3-p.x1+PIXEL-1)*c, (p.x4-p.x2+PIXEL-1)*c);
}

QPoint
to_qpoint (const coord2 & p) {
  float c= 1.0/PIXEL;
  return QPoint (p.x1*c, -p.x2*c);
}

QSize
to_qsize (const coord2 & p) {
  float c= 1.0/PIXEL;
  return QSize (p.x1*c, p.x2*c);
}

coord4
from_qrect (const QRect & rect) {
  SI c1, c2, c3, c4;
  c1= rect.x() * PIXEL;
  c2= -(rect.y() + rect.height()) * PIXEL;       
  c3= (rect.x() + rect.width()) * PIXEL;
  c4= -rect.y() * PIXEL;
  return coord4 (c1, c2, c3, c4);
}

coord2
from_qpoint (const QPoint & pt) {
  SI c1, c2;
  c1= pt.x() * PIXEL;
  c2= -pt.y() * PIXEL;
  return coord2 (c1, c2);
}

coord2
from_qsize (const QSize & s) {
  SI c1, c2;
  c1= s.width() * PIXEL;
  c2= s.height() * PIXEL;
  return coord2 (c1, c2);
}

QString
to_qstring (string s) {
  char* p= as_charp (s);
  QString nss (p);
  tm_delete_array (p);
  return nss;
}

string
from_qstring (const QString &s) {
  QByteArray arr= s.toUtf8 ();
  const char* cstr= arr.constData ();
  return utf8_to_cork (string ((char*) cstr));
}

QString
to_qstring_utf8 (string s) {
  s= cork_to_utf8 (s);
  char* p= as_charp (s);
  QString nss= QString::fromUtf8 (p, N(s));
  tm_delete_array (p);
  return nss;
}

string
qt_translate (string s) {
  string out_lan= get_output_language ();
  return tm_var_encode (translate (s, "english", out_lan));
}

bool
qt_supports_image (url u) {
  string s= suffix (u);
  if (s == "ps" || s == "eps" || s == "pdf") return false;
  return true;
}

void
qt_image_size (url image, int& w, int& h) {
  QImage im (to_qstring (sys_concretize (image)));
  if (im.isNull ()) {
    cerr << "Cannot read image file '" << image << "'" << LF;
    w= 35; h= 35;
  }
  else {
    w= im.width ();
    h= im.height ();
  }
}

void
qt_convert_image (url image, url dest) {
  QImage im (to_qstring (sys_concretize (image)));
  if (im.isNull ()) {
    cerr << "Cannot read image file '" << image << "'" << LF;
  }
  else {
    im.save (to_qstring (sys_concretize (dest)));
  }
}

string qt_application_directory ()
{
  return  string (QCoreApplication::applicationDirPath () .toAscii() .constData());
//  return from_qstring (QCoreApplication::applicationDirPath ());
}

