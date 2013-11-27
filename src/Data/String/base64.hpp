
/******************************************************************************
* MODULE     : base64.hpp
* DESCRIPTION: Implementation of the base64 coding as described by RFC-3548.
* COPYRIGHT  : (C) 2013  Francois Poulain
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "string.hpp"

#ifndef BASE64_H
#define BASE64_H

string encode_base64 (string s);
string decode_base64 (string s);

#endif // BASE64_H
