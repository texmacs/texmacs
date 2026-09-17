/******************************************************************************
* MODULE     : resvg.hpp
* DESCRIPTION: Interface with resvg
* COPYRIGHT  : (C) 2026  Liza Belos
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef RESVG_HPP
#define RESVG_HPP

#include "tm_configure.hpp"
#ifdef USE_RESVG

#include "url.hpp"
#include <resvg.h>

bool resvg_supports (url image);
void resvg_image_size (url image, int& w_pt, int& h_pt);
bool resvg_native_image_size (url image, int& w, int& h);
bool resvg_render_tree (resvg_render_tree* tree, int w, int h, char* rgba_pixels, bool fit_aspect = false);
bool resvg_render_image (url image, int w, int h, char* rgba_pixels, bool fit_aspect = false);

resvg_options* resvg_get_options ();
int resvg_parse_tree (url u, const resvg_options* opt = NULL, resvg_render_tree** tree = NULL);

// Convenience / backward-compatibility aliases
inline resvg_options* tm_get_resvg_options () {
  return resvg_get_options ();
}

inline int tm_resvg_parse_tree (url u, const resvg_options* opt = NULL, resvg_render_tree** tree = NULL) {
  return resvg_parse_tree (u, opt, tree);
}

#endif // USE_RESVG

#endif // RESVG_HPP
