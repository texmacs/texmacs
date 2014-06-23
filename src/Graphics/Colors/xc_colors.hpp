
/******************************************************************************
* MODULE     : xc_colors.h
* DESCRIPTION: RGB colors following xcolor naming conventions
* COPYRIGHT  : (C) 2014  François Poulain, Joris van der Hoeven
* NOTE       : Inspired from Dr. Uwe Kern <xcolor at ukern dot de> xcolor
*              LaTeX package
*              http://www.ctan.org/tex-archive/macros/latex/contrib/xcolor/
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#define RGB(r,g,b)  r, g, b 

static rgb_record XCColors[] = {
  {"black",           RGB (  0,   0,   0)},
  {"blue",            RGB (  0,   0, 255)},
  {"brown",           RGB (191, 127,  63)},
  {"cyan",            RGB (  0, 255, 255)},
  {"darkgray",        RGB ( 63,  63,  63)},
  {"gray",            RGB (127, 127, 127)},
  {"green",           RGB (  0, 255,   0)},
  {"lightgray",       RGB (191, 191, 191)},
  {"lime",            RGB (191, 255,   0)},
  {"magenta",         RGB (255,   0, 255)},
  {"olive",           RGB (127, 127,   0)},
  {"orange",          RGB (255, 127,   0)},
  {"pink",            RGB (255, 191, 191)},
  {"purple",          RGB (191,   0,  63)},
  {"red",             RGB (255,   0,   0)},
  {"teal",            RGB (  0, 127, 127)},
  {"violet",          RGB (127,   0, 127)},
  {"white",           RGB (255, 255, 255)},
  {"yellow",          RGB (255, 255,   0)},
  {"", 0, 0, 0}
};
