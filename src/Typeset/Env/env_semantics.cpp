
/******************************************************************************
* MODULE     : env_semantics.cpp
* DESCRIPTION: attaching numerical values to the environment variables
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "env.hpp"

/******************************************************************************
* Retrieving the page size
******************************************************************************/

/*static*/ hashmap<string,int> default_var_type (Env_User);

/*static*/ void
initialize_default_var_type () {
  if (N(default_var_type) != 0) return;
  hashmap<string,int>& var_type= default_var_type;

  var_type (PREAMBLE)         = Env_Preamble;
  var_type (DPI)              = Env_Fixed;
  var_type (SFACTOR)          = Env_Fixed;
  var_type (MAGNIFICATION)    = Env_Magnification;
  var_type (PAGE_TYPE)        = Env_Fixed;
  var_type (PAGE_WIDTH)       = Env_Fixed;
  var_type (PAGE_HEIGHT)      = Env_Fixed;
  var_type (PAGE_BREAKING)    = Env_Fixed;
  var_type (PAGE_FLEXIBILITY) = Env_Fixed;

  var_type (MODE)             = Env_Mode;
  var_type (TEXT_LANGUAGE)    = Env_Language;
  var_type (TEXT_FONT)        = Env_Font;
  var_type (TEXT_FAMILY)      = Env_Font;
  var_type (TEXT_SERIES)      = Env_Font;
  var_type (TEXT_SHAPE)       = Env_Font;
  var_type (MATH_LANGUAGE)    = Env_Language;
  var_type (MATH_FONT)        = Env_Font;
  var_type (MATH_FAMILY)      = Env_Font;
  var_type (MATH_SERIES)      = Env_Font;
  var_type (MATH_SHAPE)       = Env_Font;
  var_type (PROG_LANGUAGE)    = Env_Language;
  var_type (PROG_FONT)        = Env_Font;
  var_type (PROG_FAMILY)      = Env_Font;
  var_type (PROG_SERIES)      = Env_Font;
  var_type (PROG_SHAPE)       = Env_Font;
  var_type (FONT_BASE_SIZE)   = Env_Font_Size;
  var_type (FONT_SIZE)        = Env_Font_Size;
  var_type (INDEX_LEVEL)      = Env_Index_Level;
  var_type (DISPLAY_STYLE)    = Env_Display_Style;
  var_type (MATH_CONDENSED)   = Env_Math_Condensed;
  var_type (VERTICAL_POS)     = Env_Vertical_Pos;
  var_type (COLOR)            = Env_Color;

  var_type (PAR_MODE)         = Env_Paragraph;
  var_type (PAR_HYPHEN)       = Env_Paragraph;
  var_type (PAR_WIDTH)        = Env_Paragraph;
  var_type (PAR_LEFT)         = Env_Paragraph;
  var_type (PAR_RIGHT)        = Env_Paragraph;
  var_type (PAR_FIRST)        = Env_Paragraph;
  var_type (PAR_NO_FIRST)     = Env_Paragraph;
  var_type (PAR_SEP)          = Env_Paragraph;
  var_type (PAR_HOR_SEP)      = Env_Paragraph;
  var_type (PAR_LINE_SEP)     = Env_Paragraph;
  var_type (PAR_PAR_SEP)      = Env_Paragraph;

  var_type (PAGE_NR)          = Env_Page;
  var_type (PAGE_THE_PAGE)    = Env_Page;
  var_type (PAGE_ODD)         = Env_Page;
  var_type (PAGE_EVEN)        = Env_Page;
  var_type (PAGE_RIGHT)       = Env_Page;
  var_type (PAGE_TOP)         = Env_Page;
  var_type (PAGE_BOT)         = Env_Page;
  var_type (PAGE_SHRINK)      = Env_Page;
  var_type (PAGE_EXTEND)      = Env_Page;
  var_type (PAGE_HEAD_SEP)    = Env_Page;
  var_type (PAGE_FOOT_SEP)    = Env_Page;
  var_type (PAGE_ODD_HEADER)  = Env_Page;
  var_type (PAGE_ODD_FOOTER)  = Env_Page;
  var_type (PAGE_EVEN_HEADER) = Env_Page;
  var_type (PAGE_EVEN_FOOTER) = Env_Page;
  var_type (PAGE_THIS_HEADER) = Env_Page;
  var_type (PAGE_THIS_FOOTER) = Env_Page;
  var_type (PAGE_FNOTE_SEP)   = Env_Page;
  var_type (PAGE_FNOTE_BARLEN)= Env_Page;
  var_type (PAGE_FLOAT_SEP)   = Env_Page;
  var_type (PAGE_MNOTE_SEP)   = Env_Page;
  var_type (PAGE_MNOTE_WIDTH) = Env_Page;

  var_type (GR_FRAME)         = Env_Frame;
  var_type (GR_LINE_WIDTH)    = Env_Line_Width;
}

/******************************************************************************
* Decoding graphics
******************************************************************************/

void
edit_env_rep::get_point (tree t, SI& x, SI& y, bool& error) {
  error= true;
  if ((!is_tuple (t)) || (N(t)!=2)) return;
  double xx= as_double (t[0]);
  double yy= as_double (t[1]);
  point  p = fr (point (xx, yy));
  x= (SI) p[0];
  y= (SI) p[1];
  error= false;
}

/******************************************************************************
* Retrieving the page size
******************************************************************************/

void
edit_env_rep::get_page_pars (SI& w, SI& h, SI& width, SI& height,
			     SI& odd, SI& even, SI& top, SI& bot)
{
  SI right;
  SI rl= get_length (PAGE_REDUCE_LEFT);
  SI rr= get_length (PAGE_REDUCE_RIGHT);
  SI rt= get_length (PAGE_REDUCE_TOP);
  SI rb= get_length (PAGE_REDUCE_BOT);

  width = get_length (PAGE_WIDTH);
  height= get_length (PAGE_HEIGHT);
  odd   = get_length (PAGE_ODD);
  even  = get_length (PAGE_EVEN);
  right = get_length (PAGE_RIGHT);
  top   = get_length (PAGE_TOP);
  bot   = get_length (PAGE_BOT);

  string medium= get_string (PAGE_MEDIUM);
  if (medium != "automatic") {
    width  -= rl+rr;
    height -= rt+rb;
    odd    -= rl;
    even   -= rl;
    top    -= rt;
    bot    -= rb;

    w= get_length (PAR_WIDTH);
    h= height- top- bot;
  }
  else {
    w= width- odd- right;
    h= height- top- bot;
  }

  int nr_cols= get_int (PAR_COLUMNS);
  if (nr_cols > 1) {
    SI col_sep= get_length (PAR_COLUMNS_SEP);
    w= ((w+col_sep) / nr_cols) - col_sep;
  }

  /*
  cout << "w     = " << (w/PIXEL) << "\n";
  cout << "h     = " << (h/PIXEL) << "\n";
  cout << "width = " << (width/PIXEL) << "\n";
  cout << "height= " << (height/PIXEL) << "\n";
  cout << "odd   = " << (odd/PIXEL) << "\n";
  cout << "even  = " << (even/PIXEL) << "\n";
  cout << "right = " << (right/PIXEL) << "\n";
  cout << "top   = " << (top/PIXEL) << "\n";
  cout << "bot   = " << (bot/PIXEL) << "\n";
  */
}

/******************************************************************************
* Updating the environment from the variables
******************************************************************************/

void
edit_env_rep::update_font () {
  fn_size= (int) ((((double) get_int (FONT_BASE_SIZE))+0.5)*
		  get_double (FONT_SIZE));
  switch (mode) {
  case 0:
    fn= find_font (dis,
		   get_string (TEXT_FONT), get_string (TEXT_FAMILY),
		   get_string (TEXT_SERIES), get_string (TEXT_SHAPE),
		   script (fn_size, index_level), (int) (magn*dpi));
    break;
  case 1:
    fn= find_font (dis,
		   get_string (MATH_FONT), get_string (MATH_FAMILY),
		   get_string (MATH_SERIES), get_string (MATH_SHAPE),
		   script (fn_size, index_level), (int) (magn*dpi));
    break;
  case 2:
    fn= find_font (dis,
		   get_string (PROG_FONT), get_string (PROG_FAMILY),
		   get_string (PROG_SERIES), get_string (PROG_SHAPE),
		   script (fn_size, index_level), (int) (magn*dpi));
    break;
  }
}

void
edit_env_rep::update_color () {
  string s= get_string (COLOR);
  col= dis->get_color (s);
}

void
edit_env_rep::update_mode () {
  string s= get_string (MODE);
  if (s == "text") mode=0;
  if (s == "math") mode=1;
  if (s == "prog") mode=2;
}

void
edit_env_rep::update_language () {
  switch (mode) {
  case 0:
    lan= text_language (get_string (TEXT_LANGUAGE));
    break;
  case 1:
    lan= math_language (get_string (MATH_LANGUAGE));
    break;
  case 2:
    lan= prog_language (get_string (PROG_LANGUAGE));
    break;
  }
}

void
edit_env_rep::update_frame () {
  tree t= env [GR_FRAME];
  if (is_tuple (t, "scale", 2) && is_func (t[2], TUPLE, 2)) {
    SI magn= decode_length (as_string (t[1]));
    SI x   = decode_length (as_string (t[2][0]));
    SI y   = decode_length (as_string (t[2][1]));
    fr= scaling (magn, point (x, y));
  }
  else fr= scaling (decode_length (string ("1cm")), point (0.0, 0.0));
}

void
edit_env_rep::update () {
  magn           = get_double (MAGNIFICATION);
  index_level    = get_int (INDEX_LEVEL);
  display_style  = get_bool (DISPLAY_STYLE);
  math_condensed = get_bool (MATH_CONDENSED);
  vert_pos       = get_int (VERTICAL_POS);
  preamble       = get_bool (PREAMBLE);

  update_color ();
  update_mode ();
  update_language ();
  update_font ();

  update_frame ();
  lw= get_length (GR_LINE_WIDTH);
}

/******************************************************************************
* Update a particular changed variable
******************************************************************************/

void
edit_env_rep::update (string s) {
  switch (var_type[s]) {
  case Env_User:
    break;
  case Env_Fixed:
    break;
  case Env_Magnification:
    magn= get_double (MAGNIFICATION);
    update_font ();
    break;
  case Env_Language:
    update_language ();
    break;
  case Env_Mode:
    update_mode ();
    update_language ();
    update_font ();
    break;
  case Env_Font:
    update_font ();
    break;
  case Env_Font_Size:
    update_font ();
    break;
  case Env_Index_Level:
    index_level= get_int (INDEX_LEVEL);
    update_font ();
    break;
  case Env_Display_Style:
    display_style= get_bool (DISPLAY_STYLE);
    break;
  case Env_Math_Condensed:
    math_condensed= get_bool (MATH_CONDENSED);
    break;
  case Env_Vertical_Pos:
    vert_pos= get_int (VERTICAL_POS);
    break;
  case Env_Color:
    update_color ();
    break;
  case Env_Paragraph:
    break;
  case Env_Page:
    break;
  case Env_Preamble:
    preamble= get_bool (PREAMBLE);
    break;
  case Env_Frame:
    update_frame ();
    break;
  case Env_Line_Width:
    lw= get_length (GR_LINE_WIDTH);
    break;
  }
}
