
/******************************************************************************
* MODULE     : colors.cpp
* DESCRIPTION: Multi format RGB based color management
* COPYRIGHT  : (C) 2014  François Poulain, Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include <string.h>
#include "colors.hpp"
#include "analyze.hpp"

#include "tm_colors.hpp"
#include "x11_colors.hpp"
#include "svg_colors.hpp"
#include "xc_colors.hpp"
#include "dvips_colors.hpp"


/******************************************************************************
* Set up colormaps
******************************************************************************/

int  pastel= 223;
bool true_colors= true;
bool init_colors= true;
bool reverse_colors= false;

void
set_true_colors (bool tc) {
  true_colors= tc;
}

void
set_reverse_colors (bool rc) {
  reverse_colors= rc;
}

bool
get_reverse_colors () {
  return reverse_colors;
}

static int CSCALES= 4;
static int CFACTOR= 5;
static int GREYS  = 16;
static int CTOTAL = (CFACTOR*CFACTOR*CFACTOR+GREYS+1);

void
set_color_attrs (int cs, int cf, int gr) {
  CSCALES= cs;
  CFACTOR= cf;
  GREYS=   gr;
  CTOTAL = (CFACTOR*CFACTOR*CFACTOR+GREYS+1);
}

void
get_color_attrs (int &cs, int &cf, int &gr, int &ct) {
  cs= CSCALES;
  cf= CFACTOR;
  gr= GREYS;
  ct= CTOTAL;
}

/******************************************************************************
* Set up colors
******************************************************************************/

#ifdef QTTEXMACS
color black      = rgb_color (  0,   0,   0);
color white      = rgb_color (255, 255, 255);
color red        = rgb_color (255,   0,   0);
color blue       = rgb_color (  0,   0, 255);
color yellow     = rgb_color (255, 255,   0);
color green      = rgb_color (  0, 255,   0);
color magenta    = rgb_color (255,   0, 255);
color orange     = rgb_color (255, 128,   0);
color brown      = rgb_color (128,  32,   0);
color pink       = rgb_color (255, 128, 128);

color light_grey = rgb_color (208, 208, 208);
color grey       = rgb_color (184, 184, 184);
color dark_grey  = rgb_color (112, 112, 112);
#else
color black, white, red, green, blue;
color yellow, magenta, orange, brown, pink;
color light_grey, grey, dark_grey;

void
initialize_colors () {
  black      = rgb_color (  0,   0,   0);
  white      = rgb_color (255, 255, 255);
  red        = rgb_color (255,   0,   0);
  blue       = rgb_color (  0,   0, 255);
  yellow     = rgb_color (255, 255,   0);
  green      = rgb_color (  0, 255,   0);
  magenta    = rgb_color (255,   0, 255);
  orange     = rgb_color (255, 128,   0);
  brown      = rgb_color (128,  32,   0);
  pink       = rgb_color (255, 128, 128);
 
  light_grey = rgb_color (208, 208, 208);
  grey       = rgb_color (184, 184, 184);
  dark_grey  = rgb_color (112, 112, 112);
  pastel     = (true_colors || GREYS == 255)? 223: 191;
}
#endif

color tm_background= rgb_color (160, 160, 160);

/******************************************************************************
* Reversed colors
******************************************************************************/

void
reverse (int& r, int& g, int& b) {
  int m= min (r, min (g, b));
  int M= max (r, max (g, b));
  int t= (r + g + b) / 3;
  int tt= 255 - t;
  double mu= 1.0;
  // tt= 6 * tt / 7;
  if (M != m) {
    double lambda1= max (((double) (t - m)) / t,
			 ((double) (M - t)) / (255 - t));
    double lambda2= max (((double) (t - m)) / tt,
			 ((double) (M - t)) / (255 - tt));
    mu= lambda1 / lambda2;
  }
  r= (int) (tt + mu * (r - t) + 0.5);
  g= (int) (tt + mu * (g - t) + 0.5);
  b= (int) (tt + mu * (b - t) + 0.5);
}

/******************************************************************************
* Blended colors
******************************************************************************/

color
blend_colors (color fg, color bg) {
  if (((fg >> 24) & 255) == 255) return fg;
  int fR, fG, fB, fA, bR, bG, bB, bA;
  get_rgb_color (fg, fR, fG, fB, fA);
  get_rgb_color (bg, bR, bG, bB, bA);
  int rR= (bR * (255 - fA) + fR * fA) / 255;
  int rG= (bG * (255 - fA) + fG * fA) / 255;
  int rB= (bB * (255 - fA) + fB * fA) / 255;
  int rA= (bA * (255 - fA) + fA * fA) / 255;
  return rgb_color (rR, rG, rB, rA);
}

/******************************************************************************
* RGB interface
******************************************************************************/

color
rgb_color (int r, int g, int b, int a) {
  if (true_colors) {
    if (reverse_colors) reverse (r, g, b);
    return (a << 24) + (r << 16) + (g << 8) + b;
  }
  else if ((r==g) && (g==b))
    return (a << 24) + ((r*GREYS+ 128)/255);
  else {
    r= (r*CSCALES+ 128)/255;
    g= (g*CSCALES+ 128)/255;
    b= (b*CSCALES+ 128)/255;
    return (a << 24) + r*CFACTOR*CFACTOR + g*CFACTOR + b + GREYS + 1;
  }
}

color
rgb_color (array<int> rgba) {
  return rgb_color (N(rgba)>=1? rgba[0]: 255,
                    N(rgba)>=2? rgba[1]: 255,
                    N(rgba)>=3? rgba[2]: 255,
                    N(rgba)>=4? rgba[3]: 255);
}

void
get_rgb_color (color col, int& r, int& g, int& b, int& a) {
  if (true_colors) {
    a= (col >> 24) & 255;
    r= (col >> 16) & 255;
    g= (col >> 8 ) & 255;
    b=  col        & 255;
    if (reverse_colors) reverse (r, g, b);
  }
  else {
    a= (col >> 24) & 255;
    col= col & 0xffffff;
    if (col <= ((color) GREYS)) {
      r= (col*255)/GREYS;
      g= (col*255)/GREYS;
      b= (col*255)/GREYS;
    }
    else {
      int rr, gg, bb;
      col-= (GREYS+1);
      bb  = col % CFACTOR;
      gg  = (col/CFACTOR) % CFACTOR;
      rr  = (col/(CFACTOR*CFACTOR)) % CFACTOR;
      r   = (rr*255)/CSCALES;
      g   = (gg*255)/CSCALES;
      b   = (bb*255)/CSCALES;
    }
  }
}

array<int>
get_rgb_color (color col) {
  int r, g, b, a;
  get_rgb_color (col, r, g, b, a);
  array<int> ret;
  ret << r << g << b << a;
  return ret;
}

/******************************************************************************
* CMYK interface
******************************************************************************/

color
cmyk_color (int c, int m, int y, int k, int a) {
  double c_= c, m_= m, y_= y, k_= k;
  unsigned int r, g, b;
  c_= k_ + c_*(1 - k_/255);
  m_= k_ + m_*(1 - k_/255);
  y_= k_ + y_*(1 - k_/255);

  r= round (255 - c_);
  g= round (255 - m_);
  b= round (255 - y_);
  return rgb_color (r, g, b, a);
}
// TODO
// void
// get_cmyk_color (color col, int& c, int& m, int& y, int &k, int& a) {

/******************************************************************************
* XPM interface
******************************************************************************/

color
xpm_color (string s) {
  if (s == "none") return rgb_color (100, 100, 100, 0);
  if ((N(s) == 4) && (s[0]=='#')) {
    int r= 17 * from_hexadecimal (s (1, 2));
    int g= 17 * from_hexadecimal (s (2, 3));
    int b= 17 * from_hexadecimal (s (3, 4));
    return rgb_color (r,g,b);
  }
  if ((N(s) == 7) && (s[0]=='#')) {
    int r= from_hexadecimal (s (1, 3));
    int g= from_hexadecimal (s (3, 5));
    int b= from_hexadecimal (s (5, 7));
    return rgb_color (r,g,b);
  }
  if ((N(s) == 13) && (s[0]=='#')) {
    int r= from_hexadecimal (s (1, 5));
    int g= from_hexadecimal (s (5, 9));
    int b= from_hexadecimal (s (9, 13));
    return rgb_color (r,g,b);
  }
  s= locase_all (s);
  if (x11_ch->contains (s))
    return x11_color (s);
  return black;
}

// TODO
// void
// get_xpm_color (color col, string s) {

/******************************************************************************
* Named colors
******************************************************************************/

static void
populates_colorhash_from_rgb_record (rgb_record *rec, colorhash ch) {
  while (strcmp (rec->name, "") != 0) {
    string name (locase_all (rec->name));
    color  col= rgb_color (rec->r, rec->g, rec->b);

    if (DEBUG_STD && ch->contains (name) && ch [name] != col) {
      debug_std << "Redefined color " << name << LF
                << get_hex_color (ch [name]) << " replaced by "
                << get_hex_color (col) << LF;
    }
    ch (name)= col;
    rec++;
  }
}

static void
populates_colorhash_from_cmyk_record (cmyk_record *rec, colorhash ch) {
  while (strcmp (rec->name, "") != 0) {
    string name (locase_all (rec->name));
    color  col= cmyk_color (rec->c, rec->m, rec->y, rec->k);

    if (DEBUG_STD && ch->contains (name) && ch [name] != col) {
      debug_std << "Redefined color " << name << LF
                << get_hex_color (ch [name]) << " replaced by "
                << get_hex_color (col) << LF;
    }
    ch (name)= col;
    rec++;
  }
}

static color
color_from_name (string s) {
  if (N(s) > 0 && s[0] == '#') {
    if (N(s) == 4) {
      int r= 17 * from_hexadecimal (s (1, 2));
      int g= 17 * from_hexadecimal (s (2, 3));
      int b= 17 * from_hexadecimal (s (3, 4));
      return rgb_color (r, g, b);
    }
    else if (N(s) == 5) {
      int r= 17 * from_hexadecimal (s (1, 2));
      int g= 17 * from_hexadecimal (s (2, 3));
      int b= 17 * from_hexadecimal (s (3, 4));
      int a= 17 * from_hexadecimal (s (4, 5));
      return rgb_color (r, g, b, a);
    }
    else if (N(s) == 7) {
      int r= from_hexadecimal (s (1, 3));
      int g= from_hexadecimal (s (3, 5));
      int b= from_hexadecimal (s (5, 7));
      return rgb_color (r, g, b);
    }
    else if (N(s) == 9) {
      int r= from_hexadecimal (s (1, 3));
      int g= from_hexadecimal (s (3, 5));
      int b= from_hexadecimal (s (5, 7));
      int a= from_hexadecimal (s (7, 9));
      return rgb_color (r, g, b, a);
    }
  }
  
  if ((N(s) > 4) && (s (1,4) == "gray") && (is_numeric (s (5, N(s))))) {
    int level, i=5;
    if (read_int(s,i,level)) {
      level = (level*255) /100;
      return rgb_color (level, level, level);
    }
  }
	
  if (init_colors) {
    populates_colorhash_from_rgb_record  (XCColors, xc_ch);
    populates_colorhash_from_rgb_record  (X11Colors, x11_ch);
    populates_colorhash_from_cmyk_record (DVIPSColors, dvips_ch);
    populates_colorhash_from_rgb_record  (SVGColors, svg_ch);
    populates_colorhash_from_rgb_record  (TMColors, tm_ch);
    init_colors= false;
  }

  if (tm_ch    ->contains (s)) return tm_color    (s);
  if (x11_ch   ->contains (s)) return x11_color   (s);
  if (svg_ch   ->contains (s)) return svg_color   (s);
  if (xc_ch    ->contains (s)) return xc_color    (s);
  if (dvips_ch ->contains (s)) return dvips_color (s);

  return black;
}

string
named_color_to_xcolormap (string s) {
  s= locase_all (s);

  if (init_colors) {
    populates_colorhash_from_rgb_record  (XCColors, xc_ch);
    populates_colorhash_from_rgb_record  (X11Colors, x11_ch);
    populates_colorhash_from_cmyk_record (DVIPSColors, dvips_ch);
    populates_colorhash_from_rgb_record  (SVGColors, svg_ch);
    populates_colorhash_from_rgb_record  (TMColors, tm_ch);
    init_colors= false;
  }

  if (xc_ch    ->contains (s)) return "xcolor";
  if (x11_ch   ->contains (s)) return "x11names";
  if (svg_ch   ->contains (s)) return "svgnames";
  if (dvips_ch ->contains (s)) return "dvipsnames";
  if (tm_ch    ->contains (s)) return "texmacs";
  return "none";
}

bool
is_color_name (string s) {
  color c= named_color (s);
  if ((c & 0xffffff) != 0) return true;
  if (s == "black" || starts (s, "#000")) return true;
  return false;
}

color
named_color (string s, int a) {
  color c= color_from_name (locase_all (s));
  a= (a * (c >> 24)) / 255;
  return (a << 24) + (c & 0xffffff);
}

array<int>
get_named_rgb_color (string s) {
  return get_rgb_color (named_color (s));
}

/******************************************************************************
* Hexadecimal colors
******************************************************************************/

string
get_hex_color (color c) {
  int r, g, b, a;
  get_rgb_color (c, r, g, b, a);
  if (a == 255)
    return "#" *
      as_hexadecimal (r, 2) *
      as_hexadecimal (g, 2) *
      as_hexadecimal (b, 2);
  else
    return "#" *
      as_hexadecimal (r, 2) *
      as_hexadecimal (g, 2) *
      as_hexadecimal (b, 2) *
      as_hexadecimal (a, 2);
}

string
get_hex_color (string s) {
  return get_hex_color (named_color (s));
}

string
named_rgb_color (array<int> rgba) {
  return get_hex_color (rgb_color (rgba));
}
