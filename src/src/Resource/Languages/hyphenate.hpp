
/******************************************************************************
* MODULE     : hyphenate.hpp
* DESCRIPTION: hyphenation by Liang's algorithm
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef HYPHENATE_H
#define HYPHENATE_H
#include "language.hpp"

hashmap<string,string> load_hyphen_table (string language_name);
array<int> get_hyphens (string s, hashmap<string,string> H);
void std_hyphenate (string s, int after, string& left, string& right, int pen);

#endif // defined HYPHENATE_H
