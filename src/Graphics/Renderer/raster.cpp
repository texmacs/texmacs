
/******************************************************************************
* MODULE     : raster.cpp
* DESCRIPTION: Raster pictures
* COPYRIGHT  : (C) 2013  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "true_color.hpp"
#include "raster.hpp"

picture::picture (int w, int h, int ox, int oy):
  rep (tm_new<raster_rep<true_color> > (picture_raster, w, h, ox, oy)) {}
