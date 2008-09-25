
/******************************************************************************
* MODULE     : qt_utilities.hpp
* DESCRIPTION: Utilities for QT
* COPYRIGHT  : (C) 2008  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "message.hpp"

#include <QRect>
#include <QSize>
#include <QPoint>
#include <QString>


typedef quadruple<SI,SI,SI,SI> coord4;
typedef pair<SI,SI> coord2;

QRect to_qrect(coord4 p);
QPoint to_qpoint(coord2 p);
QSize to_qsize(coord2 p);
coord4 from_qrect(QRect & rect);
coord2 from_qpoint(QPoint & pt);
coord2 from_qsize(QSize & s);
QString to_qstring(string s);
string from_qstring(QString & s);

/******************************************************************************
 * Type checking
 ******************************************************************************/
#pragma mark type checking

inline void
check_type_void (blackbox bb, string s) {
  if (!is_nil (bb)) {
    cerr << "\nslot type= " << s << "\n";
    fatal_error ("type mismatch", "check_type");
  }
}

template<class T> void
check_type (blackbox bb, string s) {
  if (type_box (bb) != type_helper<T>::id) {
    cerr << "\nslot type= " << s << "\n";
    fatal_error ("type mismatch", "check_type");
  }
}

template<class T1, class T2> inline void
check_type (blackbox bb, string s) {
  check_type<pair<T1,T2> > (bb, s);
}


