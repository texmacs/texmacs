
/******************************************************************************
* MODULE     : tex_files.hpp
* DESCRIPTION: manipulation of TeX font files
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef TEX_FILES_H
#define TEX_FILES_H
#include "url.hpp"

void   make_tex_tfm (string fn_name);
void   make_tex_pk (string fn_name, int dpi, int design_dpi);
void   reset_tfm_path (bool rehash= true);
void   reset_pk_path  (bool rehash= true);
void   reset_pfb_path ();
url    resolve_tex (url name);
bool   exists_in_tex (url font_name);

#endif // defined TEX_FILES_H
