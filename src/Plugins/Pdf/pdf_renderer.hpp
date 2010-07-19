
/******************************************************************************
* MODULE     : pdf_renderer.hpp
* DESCRIPTION: Renderer for printing pdf graphics
* COPYRIGHT  : (C) 2010  Massimiliano Gubinelli
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef PDF_RENDERER_H
#define PDF_RENDERER_H

#include "renderer.hpp"
#include "gui.hpp"
#include "hashmap.hpp"
#include "url.hpp"

class pdf_renderer_rep: public renderer_rep {
  static const int default_dpi= 72;

  url       pdf_file_name;
  int       dpi;
  int       nr_pages;
  string    page_type;
  bool      landscape;
  double    paper_w;
  double    paper_h;

  color     fg, bg;
  SI        lw;
  string    cfn;
  int       cfid;

  double p;
  double width, height;

  
  hashmap<string,int> tex_fonts;
  hashmap<string,int> image_resources;
  
  double to_x (SI x);
  double to_y (SI y);
  void init_page_size ();
  void select_color (color c);
  void select_line_width (SI w);
  void compile_glyph (scheme_tree t);

public:
  pdf_renderer_rep (url pdf_file_name, int dpi, int nr_pages,
	       string ptype, bool landsc, double paper_w, double paper_h);
  ~pdf_renderer_rep ();
  bool is_printer ();
  void next_page ();

  void  set_clipping (SI x1, SI y1, SI x2, SI y2, bool restore= false);
  color get_color ();
  color get_background ();
  void  set_color (color c);
  void  set_background (color c);
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

  void fetch (SI x1, SI y1, SI x2, SI y2, renderer ren, SI x, SI y);
  void new_shadow (renderer& ren);
  void delete_shadow (renderer& ren);
  void get_shadow (renderer ren, SI x1, SI y1, SI x2, SI y2);
  void put_shadow (renderer ren, SI x1, SI y1, SI x2, SI y2);
  void apply_shadow (SI x1, SI y1, SI x2, SI y2);
};

renderer pdf_renderer (url pdf_file_name, int dpi, int nr_pages= 1,
		  string page_type= "a4", bool landscape= false,
		  double paper_w= 21.0, double paper_h= 29.7);

#endif // ifdef PDF_RENDERER_H
