
/******************************************************************************
* MODULE     : gs_utilities.hpp
* DESCRIPTION: Utilities for GhostScript
* COPYRIGHT  : (C) 2010 David MICHEL
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef GS_UTILITIES_HPP
#define GS_UTILITIES_HPP

#include "tm_configure.hpp"
#ifdef USE_GS

#include "url.hpp"

void gs_image_size (url image, int& w_pt, int& h_pt);
void gs_to_png (url image, url png, int w_px, int h_px);
void gs_to_eps (url pdf, url eps);

#endif // USE_GS

#endif // GS_UTILITIES_HPP

