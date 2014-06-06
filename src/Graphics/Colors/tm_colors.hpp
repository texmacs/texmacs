  
/******************************************************************************
* MODULE     : tm_colors.h
* DESCRIPTION: RGB colors following TeXmacs naming conventions
* COPYRIGHT  : (C) 2014  François Poulain, Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#define RGB(r,g,b)  r, g, b 
#define PST pastel

static rgb_record TMColors[] = {
  {"black",          RGB (  0,   0,   0)},
  {"white",          RGB (255, 255, 255)},
  {"red",            RGB (255,   0,   0)},
  {"blue",           RGB (  0,   0, 255)},
  {"yellow",         RGB (255, 255,   0)},
  {"green",          RGB (  0, 255,   0)},
  {"magenta",        RGB (255,   0, 255)},
  {"orange",         RGB (255, 128,   0)},
  {"brown",          RGB (128,  32,   0)},
  {"pink",           RGB (255, 128, 128)},
  {"light grey",     RGB (208, 208, 208)},
  {"grey",           RGB (184, 184, 184)},
  {"dark grey",      RGB (112, 112, 112)},
  {"cyan",           RGB (  0, 255, 255)},
  {"broken white",   RGB (255, 255, PST)},
  {"darker grey",    RGB ( 64,  64,  64)},
  {"dark red",       RGB (128,   0,   0)},
  {"dark blue",      RGB (  0,   0, 128)},
  {"dark yellow",    RGB (128, 128,   0)},
  {"dark green",     RGB (  0, 128,   0)},
  {"dark magenta",   RGB (128,   0, 128)},
  {"dark cyan",      RGB (  0, 128, 128)},
  {"dark orange",    RGB (128,  64,   0)},
  {"dark brown",     RGB ( 64,  16,   0)},
  {"pastel grey",    RGB (PST, PST, PST)},
  {"pastel red",     RGB (255, PST, PST)},
  {"pastel blue",    RGB (PST, PST, 255)},
  {"pastel yellow",  RGB (255, 255, PST)},
  {"pastel green",   RGB (PST, 255, PST)},
  {"pastel magenta", RGB (255, PST, 255)},
  {"pastel cyan",    RGB (PST, 255, 255)},
  {"pastel orange",  RGB (255, PST, 2*PST-255)},
  {"pastel brown",   RGB (PST, 2*PST-255, 2*PST-255)},
  {"", 0, 0, 0}
};

#undef RGB
