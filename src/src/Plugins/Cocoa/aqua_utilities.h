
/******************************************************************************
* MODULE     : aqua_utilities.h
* DESCRIPTION: Utilities for Aqua
* COPYRIGHT  : (C) 2007  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
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


