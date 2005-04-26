
/******************************************************************************
* MODULE     : printer.hpp
* DESCRIPTION: Windows under X
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef PRINTER_H
#define PRINTER_H
#include "ps_device.hpp"
#include "display.hpp"
#include "hashmap.hpp"
#include "url.hpp"

class printer_rep: public ps_device_rep {
  display  dis;
  url      ps_file_name;
  int      dpi;
  int      nr_pages;
  string   page_type;
  bool     landscape;
  double   paper_w;
  double   paper_h;
  bool     true_type;
  string   prologue;
  string   body;
  int      cur_page;
  int      linelen;

  color    fg, bg;
  int      ncols;
  SI       lw;
  int      nwidths;
  string   cfn;
  int      nfonts;
  SI       xpos, ypos;
  bool     tex_flag;

  hashmap<string,string> defs;
  hashmap<string,string> tex_chars;
  hashmap<string,string> tex_width;
  hashmap<string,string> tex_fonts;
  hashmap<string,array<int> > tex_font_chars;

public:
  printer_rep (display dis, url ps_file_name, int dpi, int nr_pages,
	       string ptype, bool landsc, double paper_w, double paper_h);
  ~printer_rep ();
  int get_type ();

  /*********************** subroutines for printing **************************/

  void define (string comm, string defn);
  void sep ();
  void cr ();
  void print (string s);
  void print (SI x, SI y);
  void move_to (SI x, SI y);
  void select_color (color c);
  void select_line_width (SI w);

  /********************* subroutines for drawing text ************************/

  void make_tex_char (string name, QN c, glyph gl);
  void select_tex_font (string name);
  void generate_tex_fonts ();

  /******************** routines from ps_device.hpp ************************/

  color rgb (int r, int g, int b);
  void  get_rgb (color col, int& r, int& g, int& b);
  color get_color ();
  color get_color (string s);
  color get_background ();
  void  set_color (color c);
  void  set_background (color c);
  void  draw (int char_code, font_glyphs fn, SI x, SI y);
  void  set_line_style (SI w, int type=0);
  void  line (SI x1, SI y1, SI x2, SI y2);
  void  clear (SI x1, SI y1, SI x2, SI y2);
  void  fill (SI x1, SI y1, SI x2, SI y2);
  void  arc (SI x1, SI y1, SI x2, SI y2, int alpha, int delta);
  void  polygon (array<SI> x, array<SI> y);
  void  xpm (url file_name, SI x, SI y);
  void  postscript (url image, SI w, SI h, SI x, SI y,
		    int x1, int y1, int x2, int y2);

  void next_page ();
  bool check_event (int type);
  void apply_shadow (SI x1, SI y1, SI x2, SI y2);
};

ps_device printer (display dis, url ps_file_name, int dpi, int nr_pages= 1,
		   string page_type= "a4", bool landscape= false,
		   double paper_w= 21.0, double paper_h= 29.7);

#endif // defined PRINTER_H
