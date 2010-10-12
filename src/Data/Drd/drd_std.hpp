
/******************************************************************************
* MODULE     : drd_std.hpp
* DESCRIPTION: standard drd for TeXmacs; most other drd's inherit from it
* COPYRIGHT  : (C) 2003  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef DRD_STD_H
#define DRD_STD_H
#include "drd_info.hpp"

extern drd_info std_drd;
extern drd_info the_drd;
extern hashmap<string,int> STD_CODE;

inline bool std_contains (string s) { return STD_CODE->contains (s); }

void init_std_drd ();

struct with_drd {
  drd_info old_drd;
  inline with_drd (drd_info new_drd): old_drd (the_drd) { the_drd= new_drd; }
  inline ~with_drd () { the_drd= old_drd; }
};

#endif // defined DRD_STD_H
