
/******************************************************************************
* MODULE     : aqua_utilities.h
* DESCRIPTION: Utilities for Aqua
* COPYRIGHT  : (C) 2007  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "mac_cocoa.h"
#include "message.hpp"

typedef quartet<SI,SI,SI,SI> coord4;
typedef pair<SI,SI> coord2;

NSRect to_nsrect(coord4 p);
NSPoint to_nspoint(coord2 p);
NSSize to_nssize(coord2 p);
coord4 from_nsrect(NSRect rect);
coord2 from_nspoint(NSPoint pt);
coord2 from_nssize(NSSize s);
NSString *to_nsstring(string s);
NSString *to_nsstring_utf8(string s);
string from_nsstring(NSString *s);
string aqua_translate (string s);

/******************************************************************************
 * Type checking
 ******************************************************************************/
#pragma mark type checking

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


