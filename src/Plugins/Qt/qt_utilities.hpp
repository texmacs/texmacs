
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
#include <QFont>
#include <QUrl>


class QStringList;
class QKeySequence;

typedef quartet<SI,SI,SI,SI> coord4;
typedef pair<SI,SI> coord2;

void qt_dump (QObject* obj, int indent=0);

/******************************************************************************
 * Conversion of data types
 ******************************************************************************/

QColor   to_qcolor (const string& );
QColor   to_qcolor (color c);
string from_qcolor (const QColor& );
color    to_color  (const QColor& );

QRect    to_qrect (const coord4 & p);
coord4 from_qrect (const QRect & rect);

QPoint   to_qpoint (const coord2 & p);
coord2 from_qpoint (const QPoint & pt);

QSize    to_qsize (const coord2 & p);
QSize    to_qsize (const SI& w, const SI& h);
coord2 from_qsize (const QSize & s);

QFont         to_qfont (int style, QFont font);
void qt_apply_tm_style (QWidget* qwid, int style);
void qt_apply_tm_style (QWidget* qwid, int style, color c);

QSize qt_decode_length (string width, string height, 
                        const QSize& ref, const QFontMetrics& fm);

QKeySequence to_qkeysequence (string s);

QStringList     to_qstringlist (array<string> l);
array<string> from_qstringlist (const QStringList& l);

///// String conversion: Assumes UTF8 encodings both in QT and TeXmacs.

QString        to_qstring (const string& s);
string       from_qstring (const QString & s);
QString   utf8_to_qstring (const string& s);
QString latin1_to_qstring (const string& s);
string  from_qstring_utf8 (const QString & s);

/*! Returns a QString with the translation of the argument to the current
 language.
 
 NOTE: translations of gui items are always done in the scheme side using 
 (translate stuff-to-translate), and this is enabled by default for most widgets
 displaying text. We need not and must not use Qt's mechanism for translations
 nor even this function, unless the strings to be translated are hardcoded in
 our code (which is wrong of course). While parsing widgets, etc. nothing is to
 be done wrt. translations.
 */
QString qt_translate (const string& s);


#ifdef OS_MACOS
QString fromNSUrl(const QUrl &url);
#endif

/******************************************************************************
 * File formats and their conversion. Other stuff.
 ******************************************************************************/

bool qt_supports (url u);
bool qt_image_size (url image, int& w, int& h);
void qt_convert_image (url image, url dest, int w =0, int h =0);
void qt_image_to_eps (url image, url eps, int w_pt =0, int h_pt =0, int dpi= 0);
string qt_image_to_eps (url image, int w_pt =0, int h_pt =0, int dpi= 0);
void qt_image_data (url image, int& w, int&h, string& data, string& palette, string& mask);
void qt_image_to_pdf (url image, url pdf, int w_pt =0, int h_pt =0, int dpi =0);

string qt_application_directory ();
string qt_get_date (string lan, string fm);
string qt_pretty_time (int t);

bool qt_print (bool&, bool&, string&, url&, string&, string&, string&);

QPixmap as_pixmap (const QImage& im);

/******************************************************************************
 * Type checking
 ******************************************************************************/

inline void
check_type_void (blackbox bb, slot s) {
  if (!is_nil (bb)) {
    failed_error << "slot type= " << as_string(s) << LF;
    FAILED ("type mismatch");
  }
}

template<class T> inline void
check_type_id (int type_id, slot s) {
  if (type_id != type_helper<T>::id) {
    failed_error << "slot type= " << as_string(s) << LF;
    FAILED ("type mismatch");
  }
}

template<class T> void
check_type (blackbox bb, slot s) {
  if (type_box (bb) != type_helper<T>::id) {
    failed_error << "slot type= " << as_string(s) << LF;
    FAILED ("type mismatch");
  }
}

template<class T1, class T2> inline void
check_type (blackbox bb, string s) {
  check_type<pair<T1,T2> > (bb, s);
}

/*! the run-loop should exit when the number of windows is zero */
extern int nr_windows; 

/******************************************************************************
 * Some debugging infrastucture
 ******************************************************************************/
extern tm_ostream& operator << (tm_ostream& out, QRect rect);
extern tm_ostream& operator << (tm_ostream& out, QSize size);

tm_ostream& operator << (tm_ostream& out, coord4 c);
tm_ostream& operator << (tm_ostream& out, coord2 c);

// deprecated, use check_type<T>(bb, slot) instead
//#define TYPE_CHECK(b) ASSERT (b, "type mismatch")   

#define NOT_IMPLEMENTED(x) \
{ if (DEBUG_QT) debug_qt << x << " not implemented yet.\n"; }

#endif  // QT_UTILITIES_HPP
