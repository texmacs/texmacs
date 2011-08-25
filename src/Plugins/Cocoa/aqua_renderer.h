
/******************************************************************************
* MODULE     : aqua_renderer.hpp
* DESCRIPTION: Cocoa drawing interface class
* COPYRIGHT  : (C) 2006,2008 Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef AQUA_RENDERER_H
#define AQUA_RENDERER_H

#include "basic_renderer.hpp"

class aqua_renderer_rep:  public basic_renderer_rep {
public:
  NSGraphicsContext *context;
  
  
  aqua_renderer_rep (int w = 0, int h = 0);
  virtual ~aqua_renderer_rep ();

  void draw (int char_code, font_glyphs fn, SI x, SI y);
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
	       double cx1, double cy1, double cx2, double cy2,
               int alpha);

  /***** private section *****************************************************/
  
  void draw_clipped (NSImage *im, int w, int h, SI x, SI y);

  void begin (void* c); // c must be a CGContextRef
  void end ();

	
  NSImage *xpm_image(url file_name);

};

aqua_renderer_rep *the_aqua_renderer();

#endif // defined AQUA_RENDERER_H
