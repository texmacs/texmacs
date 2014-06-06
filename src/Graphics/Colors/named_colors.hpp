
/******************************************************************************
* MODULE     : named_colors.hpp
* DESCRIPTION: named colors defined in external dictionnaries
* COPYRIGHT  : (C) 2013  François Poulain, Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef NAMED_COLORS_H
#define NAMED_COLORS_H
#include "basic.hpp"
#include "string.hpp"

// TODO: hack / to be factorized with src/Graphics/Renderer/basic_renderer.cpp
// and x_init.cpp
inline color html_color  (string s) {return black;};
inline color dvips_color (string s) {return black;};
inline color x11_color   (string s) {return black;};
inline color base_color  (string s) {return black;};

#endif // defined NAMED_COLORS_H
