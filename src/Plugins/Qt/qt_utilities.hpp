
/******************************************************************************
* MODULE     : qt_utilities.hpp
* DESCRIPTION: Utilities for QT
* COPYRIGHT  : (C) 2008  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "message.hpp"

#include <QRect>
#include <QSize>
#include <QPoint>
#include <QString>

typedef quartet<SI,SI,SI,SI> coord4;
typedef pair<SI,SI> coord2;

QRect to_qrect (coord4 p);
QPoint to_qpoint (coord2 p);
QSize to_qsize (coord2 p);
coord4 from_qrect (QRect & rect);
coord2 from_qpoint (QPoint & pt);
coord2 from_qsize (QSize & s);
QString to_qstring (string s);
string from_qstring (const QString & s);
string qt_translate (string s);
QString to_qstring_utf8 (string s);
bool qt_supports_image (url u);
void qt_image_size (url image, int& w, int& h);

string qt_application_directory ();

/******************************************************************************
* Type checking
******************************************************************************/

inline void
check_type_void (blackbox bb, string s) {
  if (!is_nil (bb)) {
    cerr << "\nslot type= " << s << "\n";
    FAILED ("type mismatch");
  }
}

template<class T> void
check_type (blackbox bb, string s) {
  if (type_box (bb) != type_helper<T>::id) {
    cerr << "\nslot type= " << s << "\n";
    FAILED ("type mismatch");
  }
}

template<class T1, class T2> inline void
check_type (blackbox bb, string s) {
  check_type<pair<T1,T2> > (bb, s);
}


