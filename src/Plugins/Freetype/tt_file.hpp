
/******************************************************************************
* MODULE     : tt_file.hpp
* DESCRIPTION: Finding a True Type font
* COPYRIGHT  : (C) 2003  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef TT_FILE_H
#define TT_FILE_H
#include "url.hpp"

bool tt_font_exists (string name);
url  tt_font_find (string name);

#endif // TT_FILE_H
