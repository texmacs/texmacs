
/******************************************************************************
* MODULE     : page_type.hpp
* DESCRIPTION: Data base for page sizes and default settings
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef PAGE_TYPE_H
#define PAGE_TYPE_H
#include "string.hpp"

string page_get_feature (string type, string feature, bool landscape);

#endif // defined PAGE_TYPE_H
