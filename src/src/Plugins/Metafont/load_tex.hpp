
/******************************************************************************
* MODULE     : load_tex.hpp
* DESCRIPTION: Loading TeX font metrics and glyphs
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef LOAD_TEX_H
#define LOAD_TEX_H
#include "load_tfm.hpp"
#include "load_pk.hpp"

void load_tex (string family, int size, int dpi, int dsize,
	       tex_font_metric& tfm, font_glyphs& pk);

#endif // defined LOAD_TEX_H
