
/******************************************************************************
* MODULE     : basic_renderer.hpp
* DESCRIPTION: common drawing interface class
* COPYRIGHT  : (C) 2008 Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
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
	basic_character_rep (int c2, font_glyphs fng2, int sf2, color fg2, color bg2)
  : c (c2), fng (fng2), sf (sf2), fg (fg2), bg (bg2) {};
  
	friend class basic_character;
};

class basic_character {
	CONCRETE(basic_character);
	basic_character (int c=0, font_glyphs fng= font_glyphs (),
                   int sf=1, color fg= 0, color bg= 1):
  rep (new basic_character_rep (c, fng, sf, fg, bg)) {};
	operator tree ();
};
CONCRETE_CODE(basic_character);

bool operator == (basic_character xc1, basic_character xc2);
bool operator != (basic_character xc1, basic_character xc2);
int hash (basic_character xc);



/******************************************************************************
 * structure for caching images
 ******************************************************************************/

struct cache_image_element_rep: concrete_struct {
	int w,h,nr,time;
  void *ptr;
	cache_image_element_rep (int w2, int h2,  int time2, void *ptr2) :
   w(w2), h(h2), nr(0), time(time2), ptr(ptr2) {};
	virtual ~cache_image_element_rep() {};
	friend class cache_image_element;
};

class cache_image_element {
	ABSTRACT_NULL(cache_image_element);
	//basic_image (basic_surface_t* img2, SI xo2, SI yo2, int w2, int h2);
	//cache_image () 
};

ABSTRACT_NULL_CODE(cache_image_element);


/******************************************************************************
 * basic_renderer_rep
 ******************************************************************************/


class basic_renderer_rep:   public renderer_rep {
public:
  int   w, h;
  color cur_fg, cur_bg;

public:
  basic_renderer_rep (int w2 = 0, int h2 = 0) :
  w (w2), h (h2), cur_fg(black), cur_bg(white) {};
  virtual ~basic_renderer_rep () {};
  
  virtual void get_extents (int& w, int& h);
  virtual bool interrupted (bool check= false);
  
  /***** routines from renderer.hpp ******************************************/

  color rgb (int r, int g, int b);
  void  get_rgb (color col, int& r, int& g, int& b);
  color get_color ();
  // color get_color (string s);
  color get_background ();

  void  set_clipping (SI x1, SI y1, SI x2, SI y2, bool restore= false);
  void  set_color (color c);
  void  set_background (color c);

  virtual void begin (void* handle);
  virtual void end ();

  void encode (SI& x, SI& y);
  void decode (SI& x, SI& y);
  
  
#if 0
  void  draw (int char_code, font_glyphs fn, SI x, SI y);
  void  set_line_style (SI w, int type=0, bool round=true);
  void  line (SI x1, SI y1, SI x2, SI y2);
  void  lines (array<SI> x, array<SI> y);
  void  clear (SI x1, SI y1, SI x2, SI y2);
  void  fill (SI x1, SI y1, SI x2, SI y2);
  void  arc (SI x1, SI y1, SI x2, SI y2, int alpha, int delta);
  void  fill_arc (SI x1, SI y1, SI x2, SI y2, int alpha, int delta);
  void  polygon (array<SI> x, array<SI> y, bool convex=true);
  void  xpm (url file_name, SI x, SI y);
  void  image (url u, SI w, SI h, SI x, SI y,
	       double cx1, double cy1, double cx2, double cy2);
  void draw_clipped (QTMImage * im, int w, int h, SI x, SI y);
  /***** private section *****************************************************/
  
  QTMImage *xpm_image(url file_name);
  
#endif
  
  void fetch (SI x1, SI y1, SI x2, SI y2, renderer ren, SI x, SI y);
  void new_shadow (renderer& ren);
  void delete_shadow (renderer& ren);
  void get_shadow (renderer ren, SI x1, SI y1, SI x2, SI y2);
  void put_shadow (renderer ren, SI x1, SI y1, SI x2, SI y2);
  void apply_shadow (SI x1, SI y1, SI x2, SI y2);

};

typedef basic_renderer_rep* basic_renderer;

extern bool reverse_colors;
extern int CSCALES, CFACTOR, GREYS, CTOTAL;

color xpm_to_color (string s);


cache_image_element  get_image_cache (tree lookup);
void set_image_cache (tree lookup, cache_image_element ci);

#endif // defined BASIC_RENDERER_HPP
