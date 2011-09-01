
/******************************************************************************
* MODULE     : hyphenate.hpp
* DESCRIPTION: hyphenation by Liang's algorithm
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef HYPHENATE_H
#define HYPHENATE_H
#include "language.hpp"

void load_hyphen_tables (string language_name,
                         hashmap<string,string>& patterns,
                         hashmap<string,string>& hyphenations);
array<int> get_hyphens (string s,
                        hashmap<string,string> patterns,
                        hashmap<string,string> hyphenations);
void std_hyphenate (string s, int after, string& left, string& right, int pen);

#endif // defined HYPHENATE_H
