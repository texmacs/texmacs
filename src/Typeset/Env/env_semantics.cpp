
/******************************************************************************
* MODULE     : env_semantics.cpp
* DESCRIPTION: attaching numerical values to the environment variables
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "env.hpp"
#include "page_type.hpp"
#include "typesetter.hpp"

/******************************************************************************
* Retrieving the page size
******************************************************************************/

/*static*/ hashmap<string,int> default_var_type (Env_User);

/*static*/ void
initialize_default_var_type () {
  if (N(default_var_type) != 0) return;
  hashmap<string,int>& var_type= default_var_type;

  var_type (DPI)               = Env_Fixed;
  var_type (SFACTOR)           = Env_Fixed;
  var_type (PREAMBLE)          = Env_Preamble;
  var_type (SAVE_AUX)          = Env_Fixed;
  var_type (MODE)              = Env_Mode;

  var_type (FONT)              = Env_Font;
  var_type (FONT_FAMILY)       = Env_Font;
  var_type (FONT_SERIES)       = Env_Font;
  var_type (FONT_SHAPE)        = Env_Font;
  var_type (FONT_SIZE)         = Env_Font_Size;
  var_type (FONT_BASE_SIZE)    = Env_Font_Size;
  var_type (MAGNIFICATION)     = Env_Magnification;
  var_type (COLOR)             = Env_Color;
  var_type (LANGUAGE)          = Env_Language;

  var_type (MATH_LANGUAGE)     = Env_Language;
  var_type (MATH_FONT)         = Env_Font;
  var_type (MATH_FONT_FAMILY)  = Env_Font;
  var_type (MATH_FONT_SERIES)  = Env_Font;
  var_type (MATH_FONT_SHAPE)   = Env_Font;
  var_type (MATH_LEVEL)        = Env_Index_Level;
  var_type (MATH_DISPLAY)      = Env_Display_Style;
  var_type (MATH_CONDENSED)    = Env_Math_Condensed;
  var_type (MATH_VPOS)         = Env_Vertical_Pos;

  var_type (PROG_LANGUAGE)     = Env_Language;
  var_type (PROG_FONT)         = Env_Font;
  var_type (PROG_FONT_FAMILY)  = Env_Font;
  var_type (PROG_FONT_SERIES)  = Env_Font;
  var_type (PROG_FONT_SHAPE)   = Env_Font;

  var_type (PAR_MODE)          = Env_Paragraph;
  var_type (PAR_HYPHEN)        = Env_Paragraph;
  var_type (PAR_WIDTH)         = Env_Paragraph;
  var_type (PAR_LEFT)          = Env_Paragraph;
  var_type (PAR_RIGHT)         = Env_Paragraph;
  var_type (PAR_FIRST)         = Env_Paragraph;
  var_type (PAR_NO_FIRST)      = Env_Paragraph;
  var_type (PAR_SEP)           = Env_Paragraph;
  var_type (PAR_HOR_SEP)       = Env_Paragraph;
  var_type (PAR_VER_SEP)       = Env_Paragraph;
  var_type (PAR_LINE_SEP)      = Env_Paragraph;
  var_type (PAR_PAR_SEP)       = Env_Paragraph;

  var_type (PAGE_TYPE)         = Env_Fixed;
  var_type (PAGE_BREAKING)     = Env_Fixed;
  var_type (PAGE_FLEXIBILITY)  = Env_Fixed;
  var_type (PAGE_WIDTH)        = Env_Page_Extents;
  var_type (PAGE_HEIGHT)       = Env_Page_Extents;
  var_type (PAGE_WIDTH_MARGIN) = Env_Page;
  var_type (PAGE_SCREEN_MARGIN)= Env_Page;
  var_type (PAGE_NR)           = Env_Page;
  var_type (PAGE_THE_PAGE)     = Env_Page;
  var_type (PAGE_ODD)          = Env_Page;
  var_type (PAGE_EVEN)         = Env_Page;
  var_type (PAGE_RIGHT)        = Env_Page;
  var_type (PAGE_TOP)          = Env_Page;
  var_type (PAGE_BOT)          = Env_Page;
  var_type (PAGE_USER_HEIGHT)  = Env_Page;
  var_type (PAGE_ODD_SHIFT)    = Env_Page;
  var_type (PAGE_EVEN_SHIFT)   = Env_Page;
  var_type (PAGE_SHRINK)       = Env_Page;
  var_type (PAGE_EXTEND)       = Env_Page;
  var_type (PAGE_HEAD_SEP)     = Env_Page;
  var_type (PAGE_FOOT_SEP)     = Env_Page;
  var_type (PAGE_ODD_HEADER)   = Env_Page;
  var_type (PAGE_ODD_FOOTER)   = Env_Page;
  var_type (PAGE_EVEN_HEADER)  = Env_Page;
  var_type (PAGE_EVEN_FOOTER)  = Env_Page;
  var_type (PAGE_THIS_HEADER)  = Env_Page;
  var_type (PAGE_THIS_FOOTER)  = Env_Page;
  var_type (PAGE_FNOTE_SEP)    = Env_Page;
  var_type (PAGE_FNOTE_BARLEN) = Env_Page;
  var_type (PAGE_FLOAT_SEP)    = Env_Page;
  var_type (PAGE_MNOTE_SEP)    = Env_Page;
  var_type (PAGE_MNOTE_WIDTH)  = Env_Page;

  var_type (POINT_STYLE)       = Env_Point_Style;
  var_type (LINE_WIDTH)        = Env_Line_Width;
  var_type (DASH_STYLE)        = Env_Dash_Style;
  var_type (DASH_STYLE_UNIT)   = Env_Dash_Style_Unit;
  var_type (FILL_COLOR)        = Env_Fill_Color;
  var_type (LINE_ARROWS)       = Env_Line_Arrows;
  var_type (TEXTAT_HALIGN)     = Env_Textat_Halign;
  var_type (TEXTAT_VALIGN)     = Env_Textat_Valign;
  var_type (GR_FRAME)          = Env_Frame;
  var_type (GR_GEOMETRY)       = Env_Geometry;
  var_type (GR_GRID)           = Env_Grid;
  var_type (GR_GRID_ASPECT)    = Env_Grid_Aspect;
  var_type (GR_EDIT_GRID)        = Env_Grid;
  var_type (GR_EDIT_GRID_ASPECT) = Env_Grid_Aspect;

  var_type (SRC_STYLE)         = Env_Src_Style;
  var_type (SRC_SPECIAL)       = Env_Src_Special;
  var_type (SRC_COMPACT)       = Env_Src_Compact;
  var_type (SRC_CLOSE)         = Env_Src_Close;
}

/******************************************************************************
* Retrieving the page size
******************************************************************************/

#define get_page_par(which) \
  (get_string (which) == "auto"? \
   as_length (page_get_feature (page_type, which, page_landscape)): \
   get_length (which))

void
edit_env_rep::update_page_pars () {
  page_type         = get_string (PAGE_TYPE);
  page_landscape    = (get_string (PAGE_ORIENTATION) == "landscape");
  page_automatic    = (get_string (PAGE_MEDIUM) == "automatic");
  string width_flag = get_string (PAGE_WIDTH_MARGIN);
  string height_flag= get_string (PAGE_HEIGHT_MARGIN);
  bool   screen_flag= get_bool   (PAGE_SCREEN_MARGIN);

  if (page_automatic) {
    page_width        = get_length (PAGE_SCREEN_WIDTH);
    page_height       = get_length (PAGE_SCREEN_HEIGHT);
    page_odd_margin   = get_length (PAGE_SCREEN_LEFT);
    page_right_margin = get_length (PAGE_SCREEN_RIGHT);
    page_even_margin  = page_odd_margin;
    page_top_margin   = get_length (PAGE_SCREEN_TOP);
    page_bottom_margin= get_length (PAGE_SCREEN_BOT);
    page_user_width   = page_width - page_odd_margin - page_right_margin;
    page_user_height  = page_height - page_top_margin - page_bottom_margin;
  }
  else {
    page_width        = get_page_par (PAGE_WIDTH);
    page_height       = get_page_par (PAGE_HEIGHT);

    if (width_flag == "false") {
      page_odd_margin   = get_page_par (PAGE_ODD);
      page_even_margin  = get_page_par (PAGE_EVEN);
      page_right_margin = get_page_par (PAGE_RIGHT);
      page_user_width   = page_width - page_odd_margin - page_right_margin;
    }
    else if (width_flag == "true") {
      page_user_width   = get_page_par (PAR_WIDTH);
      SI odd_sh         = get_length (PAGE_ODD_SHIFT);
      SI even_sh        = get_length (PAGE_EVEN_SHIFT);
      page_odd_margin   = ((page_width - page_user_width) >> 1) + odd_sh;
      page_even_margin  = ((page_width - page_user_width) >> 1) + even_sh;
      page_right_margin = page_width - page_odd_margin - page_user_width;
    }
    else {
      page_odd_margin   = get_page_par (PAGE_ODD);
      page_even_margin  = get_page_par (PAGE_EVEN);
      page_user_width   = get_page_par (PAR_WIDTH);
      page_right_margin = page_width - page_odd_margin - page_user_width;
    }

    if (height_flag == "false") {
      page_top_margin   = get_page_par (PAGE_TOP);
      page_bottom_margin= get_page_par (PAGE_BOT);
      page_user_height  = page_height - page_top_margin - page_bottom_margin;
    }
    else if (height_flag == "true") {
      page_user_height  = get_length (PAGE_USER_HEIGHT);
      page_top_margin   = (page_height - page_user_width) >> 1;
      page_bottom_margin= page_top_margin;
    }
    else {
      page_user_height  = get_length (PAGE_USER_HEIGHT);
      page_top_margin   = get_page_par (PAGE_TOP);
      page_bottom_margin= page_height - page_top_margin - page_user_height;
    }

    if (screen_flag) {
      page_odd_margin   = get_length (PAGE_SCREEN_LEFT);
      page_right_margin = get_length (PAGE_SCREEN_RIGHT);
      page_top_margin   = get_length (PAGE_SCREEN_TOP);
      page_bottom_margin= get_length (PAGE_SCREEN_BOT);
      page_even_margin  = page_odd_margin;
      page_width = page_user_width + page_odd_margin + page_right_margin;
      page_height= page_user_height + page_top_margin + page_bottom_margin;
    }
  }
}

void
edit_env_rep::get_page_pars (SI& w, SI& h, SI& width, SI& height,
			     SI& odd, SI& even, SI& top, SI& bot)
{
  w     = page_user_width;
  h     = page_user_height;
  width = page_width;
  height= page_height;
  odd   = page_odd_margin;
  even  = page_even_margin;
  top   = page_top_margin;
  bot   = page_bottom_margin;

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
  case 1:
    fn= find_font (get_string (FONT), get_string (FONT_FAMILY),
		   get_string (FONT_SERIES), get_string (FONT_SHAPE),
		   script (fn_size, index_level), (int) (magn*dpi));
    break;
  case 2:
    fn= find_font (get_string (MATH_FONT), get_string (MATH_FONT_FAMILY),
		   get_string (MATH_FONT_SERIES), get_string (MATH_FONT_SHAPE),
		   script (fn_size, index_level), (int) (magn*dpi));
    break;
  case 3:
    fn= find_font (get_string (PROG_FONT), get_string (PROG_FONT_FAMILY),
		   get_string (PROG_FONT_SERIES), get_string (PROG_FONT_SHAPE),
		   script (fn_size, index_level), (int) (magn*dpi));
    break;
  }
}

void
edit_env_rep::update_color () {
  string c= get_string (COLOR);
  string fc= get_string (FILL_COLOR);
  if (c == "none") {
    if (fc == "none") fill_mode= FILL_MODE_NOTHING;
    else fill_mode= FILL_MODE_INSIDE;
  }
  else {
    if (fc == "none") fill_mode= FILL_MODE_NONE;
    else fill_mode= FILL_MODE_BOTH;
  }
  col= named_color (c);
  fill_color= named_color (fc);
}

void
edit_env_rep::update_mode () {
  string s= get_string (MODE);
  if (s == "text") mode=1;
  else if (s == "math") mode=2;
  else if (s == "prog") mode=3;
  else mode=0;
}

void
edit_env_rep::update_language () {
  switch (mode) {
  case 0:
  case 1:
    lan= text_language (get_string (LANGUAGE));
    break;
  case 2:
    lan= math_language (get_string (MATH_LANGUAGE));
    break;
  case 3:
    lan= prog_language (get_string (PROG_LANGUAGE));
    break;
  }
  hl_lan= lan->hl_lan;
}

void
edit_env_rep::update_geometry () {
  tree t= env [GR_GEOMETRY];
  gw= as_length ("1par");
  gh= as_length ("0.6par");
  gvalign= as_string ("center");
  if (is_tuple (t, "geometry", 2) || is_tuple (t, "geometry", 3)) {
    if (is_length (as_string (t[1]))) gw= as_length (t[1]);
    if (is_length (as_string (t[2]))) gh= as_length (t[2]);
    if (is_tuple (t, "geometry", 3))
      gvalign= as_string (t[3]);
  }
  update_frame ();
}

void
edit_env_rep::update_frame () {
  tree t= env [GR_FRAME];
  SI yinc= gvalign == "top"    ? - gh
	 : gvalign == "bottom" ? 0
	 : - gh / 2;
  if (is_tuple (t, "scale", 2) && is_func (t[2], TUPLE, 2)) {
    SI magn= as_length (t[1]);
    SI x   = as_length (t[2][0]);
    SI y   = as_length (t[2][1]);
    if (gvalign == "top") yinc += as_length ("1ex");
    fr= scaling (magn, point (x, y + yinc));
  }
  else {
    SI cm   = as_length (string ("1cm"));
    SI par  = as_length (string ("1par"));
    SI yfrac= as_length (string ("1yfrac"));
    fr= scaling (cm, point (par >> 1, yfrac + yinc));
  }
  point p0= fr (as_point (tuple ("0par", "0par")));
  if (gvalign == "top") {
    clip_lim1= fr [point (p0[0], p0[1] - gh + as_length ("1ex"))];
    clip_lim2= fr [point (p0[0] + gw, p0[1] + as_length ("1ex"))];
  }
  else
  if (gvalign == "bottom") {
    clip_lim1= fr [point (p0[0], p0[1])];
    clip_lim2= fr [point (p0[0] + gw, p0[1] + gh)];
  }
  else {
    clip_lim1= fr [point (p0[0], p0[1] - gh/2)];
    clip_lim2= fr [point (p0[0] + gw, p0[1] + gh/2)];
  }
}

void
edit_env_rep::update_src_style () {
  string s= as_string (env [SRC_STYLE]);
  if (s == "angular") src_style= STYLE_ANGULAR;
  else if (s == "scheme") src_style= STYLE_SCHEME;
  else if (s == "latex") src_style= STYLE_LATEX;
  else if (s == "functional") src_style= STYLE_FUNCTIONAL;
}

void
edit_env_rep::update_src_special () {
  string s= as_string (env [SRC_SPECIAL]);
  if (s == "raw") src_special= SPECIAL_RAW;
  else if (s == "format") src_special= SPECIAL_FORMAT;
  else if (s == "normal") src_special= SPECIAL_NORMAL;
  else if (s == "maximal") src_special= SPECIAL_MAXIMAL;
}

void
edit_env_rep::update_src_compact () {
  string s= as_string (env [SRC_COMPACT]);
  if (s == "all") src_compact= COMPACT_ALL;
  else if (s == "inline args") src_compact= COMPACT_INLINE_ARGS;
  else if (s == "normal") src_compact= COMPACT_INLINE_START;
  else if (s == "inline") src_compact= COMPACT_INLINE;
  else if (s == "none") src_compact= COMPACT_NONE;
}

void
edit_env_rep::update_src_close () {
  string s= as_string (env [SRC_CLOSE]);
  if (s == "minimal") src_close= CLOSE_MINIMAL;
  else if (s == "compact") src_close= CLOSE_COMPACT;
  else if (s == "long") src_close= CLOSE_LONG;
  else if (s == "repeat") src_close= CLOSE_REPEAT;
}

void
edit_env_rep::update_dash_style () {
  tree t= env [DASH_STYLE];
  dash_style= array<bool>(0);
  if (is_string (t)) {
    string s= as_string (t);
    if (s == "none") {}
  }
  else
  if (is_tuple (t)) {
    int i, n= N(t);
    dash_style= array<bool> (n);
    for (i=0; i<n; i++) {
      dash_style[i]= true;
      if (t[i] == "0") dash_style[i]= false;
    }
  }
}

/*FIXME: Currently, the line-arrows property is evaluated
  only in the context of the variables which appear before
  it in the <with>. For example :

  - <with|color|blue|<with|color|green|line-arrows|<line|...>|...>
    draws green objects with green line arrows;

  - while <with|color|blue|<with|line-arrows|<line|...>|color|green|...>
    draws green objects with blue line arrows.
 */
void
edit_env_rep::update_line_arrows () {
  tree t= env [LINE_ARROWS];
  line_arrows= array<box>(2);
  if (is_string (t)) {
    string s= as_string (t);
    if (s == "none") {}
  }
  else
  if (is_tuple (t) && N(t)<=2 && N(t)>0) {
    array<box> b (2);
    b[1]= t[0]=="" ? box () : typeset_as_box (this, t[0], path(0));
    if (N(t)>=2) {
      b[0]= b[1];
      b[1]= t[1]=="" ? box () : typeset_as_box (this, t[1], path (0));
    }
    line_arrows= b;
  }
}

void
edit_env_rep::update () {
  magn           = get_double (MAGNIFICATION);
  index_level    = get_int (MATH_LEVEL);
  display_style  = get_bool (MATH_DISPLAY);
  math_condensed = get_bool (MATH_CONDENSED);
  vert_pos       = get_int (MATH_VPOS);
  preamble       = get_bool (PREAMBLE);

  update_color ();
  update_mode ();
  update_language ();
  update_font ();

  update_geometry ();
  update_frame ();
  point_style= get_string (POINT_STYLE);
  lw= get_length (LINE_WIDTH);
  update_dash_style ();
  dash_style_unit= get_length (DASH_STYLE_UNIT);
  update_line_arrows ();
  textat_halign= get_string (TEXTAT_HALIGN);
  textat_valign= get_string (TEXTAT_VALIGN);

  update_src_style ();
  update_src_special ();
  update_src_compact ();
  update_src_close ();
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
    lw= get_length (LINE_WIDTH);
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
    index_level= get_int (MATH_LEVEL);
    update_font ();
    break;
  case Env_Display_Style:
    display_style= get_bool (MATH_DISPLAY);
    break;
  case Env_Math_Condensed:
    math_condensed= get_bool (MATH_CONDENSED);
    break;
  case Env_Vertical_Pos:
    vert_pos= get_int (MATH_VPOS);
    break;
  case Env_Color:
    update_color ();
    break;
  case Env_Paragraph:
    break;
  case Env_Page:
    break;
  case Env_Page_Extents:
    update_page_pars ();
    break;
  case Env_Preamble:
    preamble= get_bool (PREAMBLE);
    break;
  case Env_Geometry:
    update_geometry ();
    break;
  case Env_Frame:
    update_frame ();
    break;
  case Env_Point_Style:
    point_style= get_string (POINT_STYLE);
    break;
  case Env_Line_Width:
    lw= get_length (LINE_WIDTH);
    break;
  case Env_Dash_Style:
    update_dash_style();
    break;
  case Env_Dash_Style_Unit:
    dash_style_unit= get_length (DASH_STYLE_UNIT);
    break;
  case Env_Fill_Color:
    update_color ();
    break;
  case Env_Line_Arrows:
    update_line_arrows();
    break;
  case Env_Textat_Halign:
    textat_halign= get_string (TEXTAT_HALIGN);
    break;
  case Env_Textat_Valign:
    textat_valign= get_string (TEXTAT_VALIGN);
    break;
  case Env_Src_Style:
    update_src_style ();
    break;
  case Env_Src_Special:
    update_src_special ();
    break;
  case Env_Src_Compact:
    update_src_compact ();
    break;
  case Env_Src_Close:
    update_src_close ();
    break;
  }
}
