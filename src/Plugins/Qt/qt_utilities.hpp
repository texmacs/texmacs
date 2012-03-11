
/******************************************************************************
* MODULE     : qt_utilities.hpp
* DESCRIPTION: Utilities for QT
* COPYRIGHT  : (C) 2008  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/
#ifndef QT_UTILITIES_HPP
#define QT_UTILITIES_HPP

#include "message.hpp"

#include <QRect>
#include <QSize>
#include <QPoint>
#include <QString>
#include <QColor>

class QStringList;

typedef quartet<SI,SI,SI,SI> coord4;
typedef pair<SI,SI> coord2;

QColor to_qcolor (const string& );
string from_qcolor (const QColor& );
QRect to_qrect (const coord4 & p);
QPoint to_qpoint (const coord2 & p);
QSize to_qsize (const coord2 & p);
coord4 from_qrect (const QRect & rect);
coord2 from_qpoint (const QPoint & pt);
coord2 from_qsize (const QSize & s);
QString to_qstylesheet(int style);
QSize qt_decode_length (string width, QWidget* qwid);

QStringList to_qstringlist(array<string> l);
array<string> from_qstringlist(const QStringList& l);
QString to_qstring (string s);
QString utf8_to_qstring (string s);  //<! convert a string with texmacs internal encoding to a QString via Utf8 encoding
string from_qstring (const QString & s);  //<! convert an utf8 texmacs string to a QString
string from_qstring_utf8 (const QString & s);  //<! convert a QString to a TeXmacs utf8 string
string qt_translate (string s); //!< convert a QString to a TeXmacs cork string
bool qt_supports (url u);
void qt_image_size (url image, int& w, int& h);
void qt_convert_image (url image, url dest, int w =0, int h =0);
void qt_image_to_eps (url image, url eps, int w_pt =0, int h_pt =0, int dpi= 0);

string qt_application_directory ();
string qt_get_date (string lan, string fm);

bool qt_print (bool&, bool&, string&, url&, string&, string&, string&);

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

extern widget the_keyboard_focus;

extern int nr_windows; 
  // the run-loop should exit when the number of windows is zero

/**
 * some debugging infrastucture
 */
extern tm_ostream& operator << (tm_ostream& out, QRect rect);
#define TYPE_CHECK(b) ASSERT (b, "type mismatch")
#define NOT_IMPLEMENTED \
{ if (DEBUG_QT) cout << "STILL NOT IMPLEMENTED\n"; }

#endif  // QT_UTILITIES_HPP
