
/******************************************************************************
* MODULE     : cg_renderer.hpp
* DESCRIPTION: CoreGraphics drawing interface class
* COPYRIGHT  : (C) 2008 Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef CG_RENDERER_HPP
#define CG_RENDERER_HPP

#include "basic_renderer.hpp"

#define ID OTHER_ID
#define outline other_outline
#include <ApplicationServices/ApplicationServices.h>
#undef ID
#undef outline

class cg_renderer_rep:  public basic_renderer_rep {
public:
  CGContextRef context;

public:
  cg_renderer_rep (int w = 0, int h = 0);
  virtual ~cg_renderer_rep ();
  
  void  draw (int char_code, font_glyphs fn, SI x, SI y);
  void  set_color (color c);
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

  void next_page ();



  /***** private section *****************************************************/

  void draw_clipped (CGImageRef im, int w, int h, SI x, SI y);	
	bool native_draw (int ch, font_glyphs fn, SI x, SI y);

  CGImageRef xpm_image(url file_name);

  
  
  void begin (void* c); // c must be a CGContextRef
  void end ();

};

cg_renderer_rep* the_cg_renderer();

#endif // defined CG_RENDERER_HPP
