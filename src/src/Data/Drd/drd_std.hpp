
/******************************************************************************
* MODULE     : drd_std.hpp
* DESCRIPTION: standard drd for TeXmacs; most other drd's inherit from it
* COPYRIGHT  : (C) 2003  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef DRD_STD_H
#define DRD_STD_H
#include "drd_info.hpp"

extern drd_info std_drd;
extern hashmap<string,int> STD_CODE;

inline int std_contains (string s) { return STD_CODE->contains (s); }

void initialize_std_drd ();

#endif // defined DRD_STD_H
