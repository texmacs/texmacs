
/******************************************************************************
* MODULE     : basic_renderer.cpp
* DESCRIPTION: common drawing interface class
* COPYRIGHT  : (C) 2008 Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#if (defined(QTTEXMACS) || defined(AQUATEXMACS))

#include "basic_renderer.hpp"
#include "analyze.hpp"
#include "gui.hpp" // for INTERRUPT_EVENT, INTERRUPTED_EVENT
#include "font.hpp" // for the definition of font
#include "rgb_colors.hpp"
#include "iterator.hpp"
#include "convert.hpp"
#include "file.hpp"
#include <string.h>

/******************************************************************************
* structure for caching font pixmaps
******************************************************************************/

basic_character::operator tree () {
  tree t (TUPLE,  as_string (rep->c), rep->fng->res_name);
  t << as_string (rep->sf) << as_string (rep->fg) << as_string (rep->bg);
  return t;
}

bool
operator == (basic_character xc1, basic_character xc2) {
  return
    (xc1->c==xc2->c) && (xc1->fng.rep==xc2->fng.rep) &&
    (xc1->sf==xc2->sf) && (xc1->fg==xc2->fg) && (xc1->bg==xc2->bg);
}

bool
operator != (basic_character xc1, basic_character xc2) {
  return
    (xc1->c!=xc2->c) || (xc1->fng.rep!=xc2->fng.rep) ||
    (xc1->sf!=xc2->sf) || (xc1->fg!=xc2->fg) || (xc1->bg!=xc2->bg);
}

int
hash (basic_character xc) {
  return xc->c ^ ((intptr_t) xc->fng.rep) ^ xc->fg ^ xc->bg ^ xc->sf;
}

/******************************************************************************
* Set up colors
******************************************************************************/

bool reverse_colors= false;

#ifdef QTTEXMACS
#define LARGE_COLORMAP
#else
#define MEDIUM_COLORMAP
#endif

#ifndef LARGE_COLORMAP
#ifdef SMALL_COLORMAP
int CSCALES= 4;
int CFACTOR= 5;
int GREYS  = 16;
#else
int CSCALES= 8;
int CFACTOR= 9;
int GREYS  = 256;
#endif
int CTOTAL = (CFACTOR*CFACTOR*CFACTOR+GREYS+1);
#endif

#ifdef LARGE_COLORMAP

color
rgb_color (int r, int g, int b, int a) {
  return (a << 24) + (r << 16) + (g << 8) + b;
}

void
get_rgb_color (color col, int& r, int& g, int& b, int& a) {
  a= (col >> 24) & 255;
  r= (col >> 16) & 255;
  g= (col >> 8) & 255;
  b= col & 255;
}

#else

color
rgb_color (int r, int g, int b, int a) {
  if ((r==g) && (g==b))
    return (a << 24) + (r*GREYS+ 128)/255;
  else {
    r= (r*CSCALES+ 128)/255;
    g= (g*CSCALES+ 128)/255;
    b= (b*CSCALES+ 128)/255;
    return (a << 24) + r*CFACTOR*CFACTOR+ g*CFACTOR+ b+ GREYS+ 1;
  }
}

void
get_rgb_color (color col, int& r, int& g, int& b, int& a) {
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

#endif

color	black   = rgb_color (0, 0, 0);
color	white   = rgb_color (255, 255, 255);
color	red     = rgb_color (255, 0, 0);
color	blue    = rgb_color (0, 0, 255);
color	yellow  = rgb_color (255, 255, 0);
color	green   = rgb_color (0, 255, 0);
color	magenta = rgb_color (255, 0, 255);
color	orange  = rgb_color (255, 128, 0);
color	brown   = rgb_color (128, 32, 0);
color	pink    = rgb_color (255, 128, 128);

color	light_grey = rgb_color (208, 208, 208);
color	grey       = rgb_color (184, 184, 184);
color	dark_grey  = rgb_color (112, 112, 112);

/******************************************************************************
* Named colors
******************************************************************************/

color
cmyk_color (unsigned int c, unsigned int m, unsigned int y, unsigned int k) {
  double c_= c, m_= m, y_= y, k_= k;
  unsigned int r, g, b;
  c_= k_ + c_*(1 - k_/255);
  m_= k_ + m_*(1 - k_/255);
  y_= k_ + y_*(1 - k_/255);

  r= round (255 - c_);
  g= round (255 - m_);
  b= round (255 - y_);
  return rgb_color (r, g, b);
}

static void
populates_colorhash_from_dictionary (string file_name, colorhash ch) {
  if (DEBUG_STD) system_info ("Loading colors: ",file_name);
  string file, name;
  color col;
  file_name = file_name * ".scm";
  if (load_string (url ("$TEXMACS_PATH/langs/colors", file_name),
        file, false)) {
    system_error ("Couldn't open encoding dictionary", file_name);
    return;
  }
  tree t = block_to_scheme_tree (file);
  if (!is_tuple (t)) {
    system_error ("Malformed encoding dictionary", file_name);
    return;
  }
  for (int i=0; i<N(t); i++) {
    if (is_func (t[i], TUPLE, 2) && is_atomic (t[i][0])
        && is_func (t[i][1], TUPLE, 3)
        && is_atomic (t[i][1][0])
        && is_atomic (t[i][1][1])
        && is_atomic (t[i][1][2])) {
      col= rgb_color (as_int (t[i][1][0]),
                      as_int (t[i][1][1]),
                      as_int (t[i][1][2]));
    }
    else if (is_func (t[i], TUPLE, 2) && is_atomic (t[i][0])
        && is_func (t[i][1], TUPLE, 4)
        && is_atomic (t[i][1][0])
        && is_atomic (t[i][1][1])
        && is_atomic (t[i][1][2])
        && is_atomic (t[i][1][3])) {
      col= cmyk_color (as_int (t[i][1][0]),
                       as_int (t[i][1][1]),
                       as_int (t[i][1][2]),
                       as_int (t[i][1][3]));
    }
    else continue;
    name= as_string (t[i][0]);
    if (name[0] == '"')         name= name (1, N(name));
    if (name[N(name)-1] == '"') name= name (0, N(name)-1);
    if (DEBUG_STD && ch->contains (name) && ch [name] != col) {
      if (DEBUG_STD); system_error ("Redefined color: ", name);
      cout << "         " << get_named_color (ch [name])
        << " replaced by " << get_named_color (col) << LF;
    }
    ch (name)= col;
  }
}

static color
named_color_bis (string s) {
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
	
  if (N(base_ch) == 0)
    populates_colorhash_from_dictionary ("base", base_ch);
  if (N(dvips_ch) == 0)
    populates_colorhash_from_dictionary ("dvips-named", dvips_ch);
  if (N(x11_ch) == 0)
    populates_colorhash_from_dictionary ("x11-named", x11_ch);
  if (N(html_ch) == 0)
    populates_colorhash_from_dictionary ("html-named", html_ch);

  if (base_ch ->contains (s)) return base_color  (s);
  if (html_ch ->contains (s)) return html_color  (s);
  if (x11_ch  ->contains (s)) return x11_color   (s);
  if (dvips_ch->contains (s)) return dvips_color (s);

  return black;
}

color
named_color (string s, int a) {
  //cout << "Get " << s << ", " << a << "\n";
  color c= named_color_bis (s);
  a= (a * (c >> 24)) / 255;
  return (a << 24) + (c & 0xffffff);
}

string
get_named_color (color c) {
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

#define RGBCOLOR(r,g,b) rgb_color(r,g,b)

color
xpm_to_color (string s) {
  if (s == "none") return rgb_color (100, 100, 100, 0);
  if ((N(s) == 4) && (s[0]=='#')) {
    int r= 17 * from_hexadecimal (s (1, 2));
    int g= 17 * from_hexadecimal (s (2, 3));
    int b= 17 * from_hexadecimal (s (3, 4));
    return RGBCOLOR(r,g,b);
  }
  if ((N(s) == 7) && (s[0]=='#')) {
    int r= from_hexadecimal (s (1, 3));
    int g= from_hexadecimal (s (3, 5));
    int b= from_hexadecimal (s (5, 7));
    return RGBCOLOR(r,g,b);
  }
  if ((N(s) == 13) && (s[0]=='#')) {
    int r= from_hexadecimal (s (1, 5));
    int g= from_hexadecimal (s (5, 9));
    int b= from_hexadecimal (s (9, 13));
    return RGBCOLOR(r,g,b);
  }
  {
    c_string name (s);
    for(int i=0; i<RGBColorsSize; i++) {
      if (strcmp(name,RGBColors[i].name)==0) {
        return RGBCOLOR(RGBColors[i].r,RGBColors[i].g,RGBColors[i].b);
      }
    }
  }
  return RGBCOLOR (0, 0, 0);
}

#undef RGBCOLOR

/******************************************************************************
* Conversion between window and postscript coordinates
******************************************************************************/

void
basic_renderer_rep::get_extents (int& w2, int& h2) {
  w2 = w; h2 = h;
}

void basic_renderer_rep::begin (void* handle) { 
  (void) handle; 
}

void basic_renderer_rep::end () {  }

/******************************************************************************
* Drawing into drawables
******************************************************************************/

color
basic_renderer_rep::rgb (int r, int g, int b, int a) {
  return rgb_color (r, g, b, a);
}

void
basic_renderer_rep::get_rgb (color col, int& r, int& g, int& b, int& a) {
  get_rgb_color (col, r, g, b, a);
}

pencil
basic_renderer_rep::get_pencil () {
  return pen;
}

brush
basic_renderer_rep::get_brush () {
  return fg_brush;
}

brush
basic_renderer_rep::get_background () {
  return bg_brush;
}

void
basic_renderer_rep::set_pencil (pencil p) {
  pen= p;
  cur_fg= pen->get_color ();
}

void
basic_renderer_rep::set_brush (brush b) {
  fg_brush= b;
  cur_fg= b->c;
}

void
basic_renderer_rep::set_background (brush b) {
  bg_brush= b;
  cur_bg= b->c;
}


void
basic_renderer_rep::set_clipping (SI x1, SI y1, SI x2, SI y2, bool restore) {
  (void) restore;
//  outer_round (x1, y1, x2, y2);
  renderer_rep::set_clipping (x1, y1, x2, y2);
}

/* shadowing and copying rectangular regions across devices defaults to nothing */

void
basic_renderer_rep::fetch (SI x1, SI y1, SI x2, SI y2,
			   renderer dev, SI x, SI y)
{
  (void) x1; (void) y1; (void) x2; (void) y2; (void) dev; (void) x; (void) y; 
  if (DEBUG_EVENTS)
    cout << "REN fetch (" << x1 << "," << x2 << "," << y1 << "," << y2
	 << ", dev ," << x << "," << y << ")\n";
}

void
basic_renderer_rep::new_shadow (renderer& dev) {
  dev = this; 
  if (DEBUG_EVENTS) cout << "REN new_shadow\n";
}

void
basic_renderer_rep::delete_shadow (renderer& dev) { dev= NULL; 
  if (DEBUG_EVENTS) cout << "REN delete_shadow\n";
}

void
basic_renderer_rep::get_shadow (renderer dev, SI x1, SI y1, SI x2, SI y2) {
  (void) x1; (void) y1; (void) x2; (void) y2; (void) dev; 
  if (DEBUG_EVENTS)
    cout << "REN get_shadow (" << x1 << "," << x2
	 << "," << y1 << "," << y2 << ", dev )\n";
}

void
basic_renderer_rep::put_shadow (renderer dev, SI x1, SI y1, SI x2, SI y2) {
  (void) x1; (void) y1; (void) x2; (void) y2; (void) dev; 
  if (DEBUG_EVENTS)
    cout << "REN put_shadow (dev, " << x1 << "," << x2
	 << "," << y1 << "," << y2 << ")\n";
}

void
basic_renderer_rep::apply_shadow (SI x1, SI y1, SI x2, SI y2) {
  (void) x1; (void) y1; (void) x2; (void) y2; 
  if (DEBUG_EVENTS)
    cout << "REN apply_shadow (" << x1 << "," << x2
	 << "," << y1 << "," << y2 << ")\n";
}

bool
gui_interrupted (bool check) {
  return check_event (check? INTERRUPT_EVENT: INTERRUPTED_EVENT);
}

#endif
