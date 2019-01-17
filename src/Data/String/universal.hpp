
/******************************************************************************
* MODULE     : universal.hpp
* DESCRIPTION: Internationalization for the universal character encoding
* COPYRIGHT  : (C) 2015  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef UNIVERSAL_H
#define UNIVERSAL_H
#include "analyze.hpp"

string uni_translit (string s);

string uni_locase_char (string s);
string uni_upcase_char (string s);
string uni_locase_first (string s);
string uni_upcase_first (string s);
string uni_locase_all (string s);
string uni_Locase_all (string s);
string uni_upcase_all (string s);

inline bool is_uni_locase_char (string s) { return uni_locase_char (s) == s; }
inline bool is_uni_upcase_char (string s) { return uni_upcase_char (s) == s; }

array<string> get_accented_list ();
string uni_unaccent_char (string s);
string uni_get_accent_char (string s);
string uni_unaccent_all (string s);

bool uni_is_letter (string s);
bool uni_before (string s1, string s2);

#endif // defined UNIVERSAL_H
