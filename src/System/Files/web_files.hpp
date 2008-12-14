
/******************************************************************************
* MODULE     : web_files.hpp
* DESCRIPTION: file handling via the web
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef WEB_FILES_H
#define WEB_FILES_H
#include "url.hpp"

void web_cache_invalidate (url u);

url get_from_web (url u);
url get_from_server (url u);
url get_from_ramdisc (url u);

bool save_to_server (url u, string s);

#endif // defined WEB_FILES_H
