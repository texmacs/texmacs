
/******************************************************************************
* MODULE     : web_files.hpp
* DESCRIPTION: file handling via the web
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef WEB_FILES_H
#define WEB_FILES_H
#include "url.hpp"

url get_from_web (url u);
url get_from_ramdisc (url u);

#endif // defined WEB_FILES_H
