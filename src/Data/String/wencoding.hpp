
/******************************************************************************
* MODULE     : wencoding.hpp
* DESCRIPTION: Makes heuristic detection of western european charsets.
* COPYRIGHT  : (C) 2012  Francois Poulain
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/
#include "string.hpp"

#ifndef WENCODING_H
#define WENCODING_H

bool looks_ascii (string s);
bool looks_utf8_with_bom (string s);
bool looks_utf8 (string s);
bool looks_iso_8859 (string s);
bool looks_universal (string s);

string guess_wencoding (string s);
string western_to_cork (string s);
string western_to_utf8 (string s);

#endif // CONVERTER_H
