
/******************************************************************************
* MODULE     : load_tex.hpp
* DESCRIPTION: Loading TeX font metrics and glyphs
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef LOAD_TEX_H
#define LOAD_TEX_H
#include "load_tfm.hpp"
#include "load_pk.hpp"

void load_tex (string family, int size, int dpi, int dsize,
	       tex_font_metric& tfm, font_glyphs& pk);

#endif // defined LOAD_TEX_H
