
/******************************************************************************
* MODULE     : basic_renderer.hpp
* DESCRIPTION: common drawing interface class
* COPYRIGHT  : (C) 2008 Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef BASIC_RENDERER_HPP
#define BASIC_RENDERER_HPP

#include "renderer.hpp"
#include "gui.hpp" // for black, white

/******************************************************************************
* structure for caching font pixmaps
******************************************************************************/

struct basic_character_rep: concrete_struct {
  int          c;
  font_glyphs  fng;
  int          sf;
  color        fg;
  color        bg;
  basic_character_rep (int c2, font_glyphs fng2, int sf2, color fg2, color bg2):
    c (c2), fng (fng2), sf (sf2), fg (fg2), bg (bg2) {};

  friend class basic_character;
};

class basic_character {
CONCRETE(basic_character);
  basic_character (int c=0, font_glyphs fng= font_glyphs (),
                   int sf=1, color fg= 0, color bg= 1):
    rep (tm_new<basic_character_rep> (c, fng, sf, fg, bg)) {};
  operator tree ();
};
CONCRETE_CODE(basic_character);

bool operator == (basic_character xc1, basic_character xc2);
bool operator != (basic_character xc1, basic_character xc2);
int hash (basic_character xc);

/******************************************************************************
* basic_renderer_rep
******************************************************************************/

class basic_renderer_rep: public renderer_rep {
public:
  int    w, h;
  pencil pen;
  brush  fg_brush;
  brush  bg_brush;

public:
  basic_renderer_rep (bool screen_flag, int w2 = 0, int h2 = 0):
    renderer_rep (screen_flag), w (w2), h (h2),
    pen (black), fg_brush (black), bg_brush (white) {}
  virtual ~basic_renderer_rep () {}
  
  virtual void get_extents (int& w, int& h);
  
  /***** routines from renderer.hpp ******************************************/

  color  rgb (int r, int g, int b, int a= 255);
  void   get_rgb (color col, int& r, int& g, int& b, int& a);
  pencil get_pencil ();
  brush  get_brush ();
  brush  get_background ();

  void  set_clipping (SI x1, SI y1, SI x2, SI y2, bool restore= false);
  void  set_pencil (pencil p);
  void  set_brush (brush b);
  void  set_background (brush b);

  virtual void begin (void* handle);
  virtual void end ();
  
  void fetch (SI x1, SI y1, SI x2, SI y2, renderer ren, SI x, SI y);
  void new_shadow (renderer& ren);
  void delete_shadow (renderer& ren);
  void get_shadow (renderer ren, SI x1, SI y1, SI x2, SI y2);
  void put_shadow (renderer ren, SI x1, SI y1, SI x2, SI y2);
  void apply_shadow (SI x1, SI y1, SI x2, SI y2);
};

typedef basic_renderer_rep* basic_renderer;

bool gui_interrupted (bool check);

#endif // defined BASIC_RENDERER_HPP
