
/******************************************************************************
* MODULE     : tex_files.hpp
* DESCRIPTION: manipulation of TeX font files
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef TEX_FILES_H
#define TEX_FILES_H
#include "boot.hpp"
#include "url.hpp"

void   make_tex_tfm (string fn_name);
void   make_tex_pk (string fn_name, int dpi, int design_dpi, string where);
void   reset_tfm_path (bool rehash= true);
void   reset_pk_path (bool rehash= true);
url    resolve_tfm (url name);
url    resolve_pk (url name);
bool   exists_in_tex (url font_name);
void   ec_to_cm (string& name, unsigned char& c);
string pk_to_true_type (string& name);

#endif // defined TEX_FILES_H
