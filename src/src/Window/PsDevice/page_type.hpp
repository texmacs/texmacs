
/******************************************************************************
* MODULE     : page_type.hpp
* DESCRIPTION: Data base for page sizes and default settings
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef PAGE_TYPE_H
#define PAGE_TYPE_H
#include "string.hpp"

string page_get_feature (string type, string feature, bool landscape);

#endif // defined PAGE_TYPE_H
