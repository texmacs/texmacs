
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
#include "Boxes/construct.hpp"
#include "analyze.hpp"

/******************************************************************************
* Retrieving the page size
******************************************************************************/

/*static*/ hashmap<string,int> default_var_type (Env_User);

/*static*/ void
initialize_default_var_type () {
  if (N(default_var_type) != 0) return;
  hashmap<string,int>& var_type= default_var_type;

  var_type (DPI)                = Env_Fixed;
  var_type (ZOOM_FACTOR)        = Env_Zoom;
  var_type (PREAMBLE)           = Env_Preamble;
  var_type (SAVE_AUX)           = Env_Fixed;
  var_type (MODE)               = Env_Mode;
  var_type (INFO_FLAG)          = Env_Info_Level;

  var_type (FONT)               = Env_Font;
  var_type (FONT_FAMILY)        = Env_Font;
  var_type (FONT_SERIES)        = Env_Font;
  var_type (FONT_SHAPE)         = Env_Font;
  var_type (FONT_SIZE)          = Env_Font_Size;
  var_type (FONT_BASE_SIZE)     = Env_Font_Size;
  var_type (FONT_EFFECTS)       = Env_Font;
  var_type (MAGNIFICATION)      = Env_Magnification;
  var_type (MAGNIFY)            = Env_Magnify;
  var_type (COLOR)              = Env_Color;
  var_type (OPACITY)            = Env_Color;
  var_type (NO_PATTERNS)        = Env_Pattern_Mode;
  var_type (LANGUAGE)           = Env_Language;
  var_type (SPACING_POLICY)     = Env_Spacing;

  var_type (MATH_LANGUAGE)      = Env_Language;
  var_type (MATH_FONT)          = Env_Font;
  var_type (MATH_FONT_FAMILY)   = Env_Font;
  var_type (MATH_FONT_SERIES)   = Env_Font;
  var_type (MATH_FONT_SHAPE)    = Env_Font;
  var_type (MATH_FONT_SIZES)    = Env_Font_Sizes;
  var_type (MATH_LEVEL)         = Env_Index_Level;
  var_type (MATH_DISPLAY)       = Env_Display_Style;
  var_type (MATH_CONDENSED)     = Env_Math_Condensed;
  var_type (MATH_VPOS)          = Env_Vertical_Pos;
  var_type (MATH_NESTING_LEVEL) = Env_Math_Nesting;
  var_type (MATH_FRAC_LIMIT)    = Env_Math_Width;
  var_type (MATH_TABLE_LIMIT)   = Env_Math_Width;
  var_type (MATH_FLATTEN_COLOR) = Env_Math_Width;

  var_type (PROG_LANGUAGE)      = Env_Language;
  var_type (PROG_FONT)          = Env_Font;
  var_type (PROG_FONT_FAMILY)   = Env_Font;
  var_type (PROG_FONT_SERIES)   = Env_Font;
  var_type (PROG_FONT_SHAPE)    = Env_Font;

  var_type (PAR_MODE)           = Env_Paragraph;
  var_type (PAR_FLEXIBILITY)    = Env_Paragraph;
  var_type (PAR_HYPHEN)         = Env_Paragraph;
  var_type (PAR_MIN_PENALTY)    = Env_Paragraph;
  var_type (PAR_SPACING)        = Env_Paragraph;
  var_type (PAR_KERNING_REDUCE) = Env_Paragraph;
  var_type (PAR_KERNING_STRETCH)= Env_Paragraph;
  var_type (PAR_KERNING_MARGIN) = Env_Paragraph;
  var_type (PAR_CONTRACTION)    = Env_Paragraph;
  var_type (PAR_EXPANSION)      = Env_Paragraph;
  var_type (PAR_HYPHEN)         = Env_Paragraph;
  var_type (PAR_WIDTH)          = Env_Paragraph;
  var_type (PAR_LEFT)           = Env_Paragraph;
  var_type (PAR_RIGHT)          = Env_Paragraph;
  var_type (PAR_FIRST)          = Env_Paragraph;
  var_type (PAR_NO_FIRST)       = Env_Paragraph;
  var_type (PAR_SEP)            = Env_Paragraph;
  var_type (PAR_HOR_SEP)        = Env_Paragraph;
  var_type (PAR_VER_SEP)        = Env_Paragraph;
  var_type (PAR_LINE_SEP)       = Env_Paragraph;
  var_type (PAR_PAR_SEP)        = Env_Paragraph;

  var_type (PAGE_TYPE)          = Env_Fixed;
  var_type (PAGE_BREAKING)      = Env_Fixed;
  var_type (PAGE_FLEXIBILITY)   = Env_Fixed;
  var_type (PAGE_FIRST)         = Env_Fixed;
  var_type (PAGE_WIDTH)         = Env_Page_Extents;
  var_type (PAGE_HEIGHT)        = Env_Page_Extents;
  var_type (PAGE_CROP_MARKS)    = Env_Page_Extents;
  var_type (PAGE_WIDTH_MARGIN)  = Env_Page;
  var_type (PAGE_SCREEN_MARGIN) = Env_Page;
  var_type (PAGE_NR)            = Env_Page;
  var_type (PAGE_THE_PAGE)      = Env_Page;
  var_type (PAGE_ODD)           = Env_Page;
  var_type (PAGE_EVEN)          = Env_Page;
  var_type (PAGE_RIGHT)         = Env_Page;
  var_type (PAGE_TOP)           = Env_Page;
  var_type (PAGE_BOT)           = Env_Page;
  var_type (PAGE_USER_HEIGHT)   = Env_Page;
  var_type (PAGE_ODD_SHIFT)     = Env_Page;
  var_type (PAGE_EVEN_SHIFT)    = Env_Page;
  var_type (PAGE_SHRINK)        = Env_Page;
  var_type (PAGE_EXTEND)        = Env_Page;
  var_type (PAGE_HEAD_SEP)      = Env_Page;
  var_type (PAGE_FOOT_SEP)      = Env_Page;
  var_type (PAGE_ODD_HEADER)    = Env_Page;
  var_type (PAGE_ODD_FOOTER)    = Env_Page;
  var_type (PAGE_EVEN_HEADER)   = Env_Page;
  var_type (PAGE_EVEN_FOOTER)   = Env_Page;
  var_type (PAGE_THIS_TOP)      = Env_Page;
  var_type (PAGE_THIS_BOT)      = Env_Page;
  var_type (PAGE_THIS_HEADER)   = Env_Page;
  var_type (PAGE_THIS_FOOTER)   = Env_Page;
  var_type (PAGE_THIS_BG_COLOR) = Env_Page;
  var_type (PAGE_FNOTE_SEP)     = Env_Page;
  var_type (PAGE_FNOTE_BARLEN)  = Env_Page;
  var_type (PAGE_FLOAT_SEP)     = Env_Page;
  var_type (PAGE_MNOTE_SEP)     = Env_Page;
  var_type (PAGE_MNOTE_WIDTH)   = Env_Page;

  var_type (POINT_STYLE)        = Env_Point_Style;
  var_type (POINT_SIZE)         = Env_Point_Size;
  var_type (POINT_BORDER)       = Env_Point_Size;
  var_type (LINE_WIDTH)         = Env_Line_Width;
  var_type (DASH_STYLE)         = Env_Dash_Style;
  var_type (DASH_STYLE_UNIT)    = Env_Dash_Style_Unit;
  var_type (FILL_COLOR)         = Env_Fill_Color;
  var_type (ARROW_BEGIN)        = Env_Line_Arrows;
  var_type (ARROW_END)          = Env_Line_Arrows;
  var_type (ARROW_LENGTH)       = Env_Line_Arrows;
  var_type (ARROW_HEIGHT)       = Env_Line_Arrows;
  var_type (LINE_PORTION)       = Env_Line_Portion;
  var_type (TEXT_AT_HALIGN)     = Env_Text_At_Halign;
  var_type (TEXT_AT_VALIGN)     = Env_Text_At_Valign;
  var_type (DOC_AT_VALIGN)      = Env_Doc_At_Valign;
  var_type (GR_FRAME)           = Env_Frame;
  var_type (GR_GEOMETRY)        = Env_Geometry;
  var_type (GR_GRID)            = Env_Grid;
  var_type (GR_GRID_ASPECT)     = Env_Grid_Aspect;
  var_type (GR_EDIT_GRID)       = Env_Grid;
  var_type (GR_EDIT_GRID_ASPECT)= Env_Grid_Aspect;

  var_type (SRC_STYLE)          = Env_Src_Style;
  var_type (SRC_SPECIAL)        = Env_Src_Special;
  var_type (SRC_COMPACT)        = Env_Src_Compact;
  var_type (SRC_CLOSE)          = Env_Src_Close;
  var_type (SRC_TAG_COLOR)      = Env_Src_Color;
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

  page_floats       = (get_string (PAGE_FLOAT_ENABLE) == "true");
  if (get_string (PAGE_FLOAT_ENABLE) == get_string (PAGE_MEDIUM))
    page_floats= true;

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
    page_width  = get_page_par (PAGE_WIDTH);
    page_height = get_page_par (PAGE_HEIGHT);

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

    if (page_type == "user") {
      if (get_string (PAGE_EVEN) == "auto" &&
          get_string (PAGE_ODD ) != "auto")
        page_even_margin= page_odd_margin;
      if (get_string (PAGE_ODD ) == "auto" &&
          get_string (PAGE_EVEN) != "auto")
        page_odd_margin= page_even_margin;
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

  string crop_marks= get_string (PAGE_CROP_MARKS);
  page_real_type  = page_type;
  page_real_width = page_width;
  page_real_height= page_height;
  if (crop_marks != "" && get_string (PAGE_MEDIUM) == "paper") {
    page_real_type= crop_marks;
    page_real_width=
      as_length (page_get_feature (crop_marks, PAGE_WIDTH, page_landscape));
    page_real_height=
      as_length (page_get_feature (crop_marks, PAGE_HEIGHT, page_landscape));
  }
  
  page_single= get_bool (PAGE_SINGLE);
  page_packet= get_int (PAGE_PACKET);
  page_offset= get_int (PAGE_OFFSET);
  page_border= read (PAGE_BORDER);
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
  cout << "top   = " << (top/PIXEL) << "\n";
  cout << "bot   = " << (bot/PIXEL) << "\n";
  cout << "cols  = " << nr_cols << "\n";
  */
}

SI
edit_env_rep::get_page_width (bool deco) {
  SI w= page_user_width + page_odd_margin + page_right_margin;
  if (get_string (PAGE_MEDIUM) == "paper" &&
      get_string (PAGE_BORDER) != "none" &&
      deco) w += 20 * pixel;
  return w;
}

SI
edit_env_rep::get_pages_width (bool deco) {
  SI w= page_user_width + page_odd_margin + page_right_margin;
  if (get_string (PAGE_MEDIUM) == "paper" &&
      get_string (PAGE_BORDER) != "attached" &&
      get_string (PAGE_BORDER) != "none" &&
      deco) w += 20 * pixel;
  w= w * page_packet;
  if (get_string (PAGE_MEDIUM) == "paper" &&
      get_string (PAGE_BORDER) == "attached" &&
      deco) w += 20 * pixel;
  return w;
}

SI
edit_env_rep::get_page_height (bool deco) {
  SI h= page_user_height + page_top_margin + page_bottom_margin;
  if (get_string (PAGE_MEDIUM) == "paper" &&
      get_string (PAGE_BORDER) != "none" &&
      deco) h += 20 * pixel;
  return h;
}

/******************************************************************************
* Retrieving ornament parameters
******************************************************************************/

static tree
tuplify (tree t) {
  array<string> a= tokenize (t->label, ",");
  tree r (TUPLE, N(a));
  for (int i=0; i<N(a); i++)
    r[i]= a[i];
  return r;
}

ornament_parameters
edit_env_rep::get_ornament_parameters () {
  tree  shape = read (ORNAMENT_SHAPE);
  tree  tst   = read (ORNAMENT_TITLE_STYLE);
  tree  bg    = read (ORNAMENT_COLOR);
  tree  xc    = read (ORNAMENT_EXTRA_COLOR);
  tree  sunny = read (ORNAMENT_SUNNY_COLOR);
  tree  shadow= read (ORNAMENT_SHADOW_COLOR);
  int   a     = alpha;
  tree  w     = read (ORNAMENT_BORDER);
  tree  ext   = read (ORNAMENT_SWELL);
  tree  cor   = read (ORNAMENT_CORNER);
  tree  xpad  = read (ORNAMENT_HPADDING);
  tree  ypad  = read (ORNAMENT_VPADDING);

  array<brush> border;
  if (is_func (sunny, TUPLE)) {
    for (int i=0; i<N(sunny); i++)
      border << brush (sunny[i], a);
  }
  else {
    border << brush (sunny, a);
    border << brush (shadow, a);
  }
  if (N(border) == 1) border << border[0];
  if (N(border) == 2) border << border[1] << border[0];

  SI lw, bw, rw, tw;
  if (is_atomic (w) && occurs (",", w->label))
    w= tuplify (w);
  if (is_func (w, TUPLE, 4)) {
    lw= ((as_length (w[0]) >> 1) << 1);
    bw= ((as_length (w[1]) >> 1) << 1);
    rw= ((as_length (w[2]) >> 1) << 1);
    tw= ((as_length (w[3]) >> 1) << 1);
  }
  else lw= bw= rw= tw= ((as_length (w) >> 1) << 1);

  double lx, bx, rx, tx;
  if (is_atomic (ext) && occurs (",", ext->label))
    ext= tuplify (ext);
  if (is_func (ext, TUPLE, 4)) {
    lx= as_double (ext[0]);
    bx= as_double (ext[1]);
    rx= as_double (ext[2]);
    tx= as_double (ext[3]);
  }
  else lx= bx= rx= tx= as_double (ext);

  SI lpad, rpad;
  if (is_atomic (xpad) && occurs (",", xpad->label))
    xpad= tuplify (xpad);
  if (is_func (xpad, TUPLE, 2)) {
    lpad= as_length (xpad[0]);
    rpad= as_length (xpad[1]);
  }
  else lpad= rpad= as_length (xpad);

  SI bpad, tpad;
  if (is_atomic (ypad) && occurs (",", ypad->label))
    ypad= tuplify (ypad);
  if (is_func (ypad, TUPLE, 2)) {
    bpad= as_length (ypad[0]);
    tpad= as_length (ypad[1]);
  }
  else bpad= tpad= as_length (ypad);


  double corf= 1.0;
  if (shape != "classic") {
    if (shape == "rounded") corf= 2.0;
    if (shape == "angular") corf= 1.5;
  }
  
  SI lcor, bcor, rcor, tcor;
  if (is_percentage (cor)) {
    double a= as_percentage (cor) * corf;
    lcor= rcor= (SI) round (a * min (lpad, rpad));
    bcor= tcor= (SI) round (a * min (bpad, tpad));
  }
  else if (is_atomic (cor) && !occurs (",", cor->label))
    lcor= bcor= rcor= tcor= as_length (cor);
  else {
    if (is_atomic (cor))
      cor= tuplify (cor);
    if (is_func (cor, TUPLE, 2))
      cor= tuple (cor[0], cor[1], cor[0], cor[1]);
    if (!is_func (cor, TUPLE, 4))
      lcor= bcor= rcor= tcor= 0;
    else {
      if (is_percentage (cor[0]))
        lcor= (SI) round (as_percentage (cor[0]) * corf * lpad);
      else lcor= as_length (cor[0]);
      if (is_percentage (cor[1])) 
        bcor= (SI) round (as_percentage (cor[1]) * corf * bpad);
      else bcor= as_length (cor[1]);
      if (is_percentage (cor[2])) 
        rcor= (SI) round (as_percentage (cor[2]) * corf * rpad);
      else rcor= as_length (cor[2]);
      if (is_percentage (cor[3])) 
        tcor= (SI) round (as_percentage (cor[3]) * corf * tpad);
      else tcor= as_length (cor[3]);
    }
  }
  
  return ornament_parameters (shape, tst,
                              lw, bw, rw, tw,
			      lx, bx, rx, tx,
			      lpad, bpad, rpad, tpad,
                              lcor, bcor, rcor, tcor,
                              brush (bg, a), brush (xc, a), border);
}

art_box_parameters
edit_env_rep::get_art_box_parameters (tree t) {
  tree data (TUPLE);
  SI lpad= 0, rpad= 0, bpad= 0, tpad= 0;
  for (int i=1; i<N(t); i++)
    if (is_func (t[i], TUPLE) && N(t[i]) >= 2) {
      tree u= t[i], r (TUPLE);
      bool main_text= (u[0] == "text");
      for (int j=0; j+1<N(u); j+=2) {
        tree var= exec (u[j]);
        tree val= exec (u[j+1]);
        if (is_atomic (val) && is_length (val->label))
          val= as_tmlen (val);
        r << var << val;
        if (main_text) {
          if      (var == "lpadding") lpad= as_length (val);
          else if (var == "rpadding") rpad= as_length (val);
          else if (var == "bpadding") bpad= as_length (val);
          else if (var == "tpadding") tpad= as_length (val);
        }
      }
      data << r;
    }
  return art_box_parameters (data, lpad, bpad, rpad, tpad);
}

/******************************************************************************
* Various font sizes for scripts
******************************************************************************/

static array<int>
determine_sizes (tree szt, int sz) {
  array<int> r;
  r << sz;
  if (is_tuple (szt))
    for (int i=0; i<N(szt); i++)
      if (is_tuple (szt[i]) && N(szt[i]) >= 1)
	if (szt[i][0] == "all" ||
	    szt[i][0] == as_string (sz)) {
	  for (int j=1; j<N(szt[i]); j++)
	    if (is_atomic (szt[i][j])) {
	      string s= szt[i][j]->label;
	      if (is_int (s)) r << as_int (s);
	      else if (starts (s, "*")) {
		s= s (1, N(s));
		double x= 1.0;
		int d= search_forwards ("/", s);
		if (d >= 0 && is_double (s(0, d)) && is_double (s(d+1, N(s))))
		  x= as_double (s (0, d)) / as_double (s (d+1, N(s)));
		else if (is_double (s)) x= as_double (s);
		int xsz= (int) ceil ((x - 0.001) * sz);
		r << xsz;
	      }
	    }
	  //cout << szt << ", " << sz << " -> " << r << LF;
	  return r;
	}
  r << script (sz, 1);
  r << script (sz, 2);
  //cout << szt << ", " << sz << " -> " << r << LF;
  return r;
}

int
edit_env_rep::get_script_size (int sz, int level) {
  while (sz >= N(size_cache)) {
    int xsz= N(size_cache);
    size_cache << determine_sizes (math_font_sizes, xsz);
  }
  array<int>& a (size_cache[sz]);
  if (level < N(a)) return a[level];
  else return a[N(a) - 1];
}

/******************************************************************************
* Updating the environment from the variables
******************************************************************************/

void
edit_env_rep::update_font () {
  fn_size= (int) (((double) get_int (FONT_BASE_SIZE)) *
		  get_double (FONT_SIZE) + 0.5);
  switch (mode) {
  case 0:
  case 1:
    fn= smart_font (get_string (FONT), get_string (FONT_FAMILY),
                    get_string (FONT_SERIES), get_string (FONT_SHAPE),
                    get_script_size (fn_size, index_level), (int) (magn*dpi));
    break;
  case 2:
    fn= smart_font (get_string (MATH_FONT), get_string (MATH_FONT_FAMILY),
                    get_string (MATH_FONT_SERIES), get_string (MATH_FONT_SHAPE),
                    get_string (FONT), get_string (FONT_FAMILY),
                    get_string (FONT_SERIES), "mathitalic",
                    get_script_size (fn_size, index_level), (int) (magn*dpi));
    break;
  case 3:
    fn= smart_font (get_string (PROG_FONT), get_string (PROG_FONT_FAMILY),
                    get_string (PROG_FONT_SERIES), get_string (PROG_FONT_SHAPE),
                    get_string (FONT), get_string (FONT_FAMILY) * "-tt",
                    get_string (FONT_SERIES), get_string (FONT_SHAPE),
                    get_script_size (fn_size, index_level), (int) (magn*dpi));
    break;
  }
  string eff= get_string (FONT_EFFECTS);
  if (N(eff) != 0) fn= apply_effects (fn, eff);
}

int
decode_alpha (string s) {
  if (N(s) == 0) return 255;
  else if (s[N(s)-1] == '%') {
    // mg: be careful to avoid rounding problems for the conversion from double to int : (int)(2.55*100.0)=254
    double p = as_double (s (0, N(s)-1));
    if (p<0.0) p = 0.0;
    if (p>100.0) p = 100.0;
    return ((int) (255.0 * p))/100;
  }
  else {
    double p = as_double (s);
    if (p<0.0) p = 0.0;
    if (p>1.0) p = 1.0;
    return ((int) (255.0 * p));
  }
}

void
edit_env_rep::update_color () {
  alpha= decode_alpha (get_string (OPACITY));
  tree pc= env [COLOR];
  tree fc= env [FILL_COLOR];
  if (pc == "none") pen= pencil (false);
  else {
    if (L(pc) == PATTERN) pc= exec (pc);
    pen= pencil (pc, alpha, get_length (LINE_WIDTH));
  }
  if (fc == "none") fill_brush= brush (false);
  else {
    if (L(fc) == PATTERN) fc= exec (fc);
    fill_brush= brush (fc, alpha);
  }
}

void
edit_env_rep::update_pattern_mode () {
  no_patterns= (get_string (NO_PATTERNS) == "true");
  if (no_patterns) {
    tree c= env[COLOR];
    if (is_func (c, PATTERN, 4)) env (COLOR)= exec (c);
    c= env[BG_COLOR];
    if (is_func (c, PATTERN, 4)) env (BG_COLOR)= exec (c);
    c= env[FILL_COLOR];
    if (is_func (c, PATTERN, 4)) env (FILL_COLOR)= exec (c);
    c= env[ORNAMENT_COLOR];
    if (is_func (c, PATTERN, 4)) env (ORNAMENT_COLOR)= exec (c);
    c= env[ORNAMENT_EXTRA_COLOR];
    if (is_func (c, PATTERN, 4)) env (ORNAMENT_EXTRA_COLOR)= exec (c);
    update_color ();
  }
}

void
edit_env_rep::update_mode () {
  string s= get_string (MODE);
  if (s == "text") mode=1;
  else if (s == "math") mode=2;
  else if (s == "prog") mode=3;
  else mode=0;
  if (mode == 2) mode_op= OP_SYMBOL;
  else mode_op= OP_TEXT;
}

void
edit_env_rep::update_info_level () {
  string s= get_string (INFO_FLAG);
  if (s == "none") info_level= INFO_NONE;
  else if (s == "minimal") info_level= INFO_MINIMAL;
  else if (s == "short") info_level= INFO_SHORT;
  else if (s == "detailed") info_level= INFO_DETAILED;
  else if (s == "paper") info_level= INFO_PAPER;
  else if (s == "short-paper") info_level= INFO_SHORT_PAPER;
  else info_level= INFO_MINIMAL;
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
    if (is_length (as_string (t[1])) || is_anylen (t[1]))
      gw= as_length (t[1]);
    if (is_length (as_string (t[2])) || is_anylen (t[2]))
      gh= as_length (t[2]);
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
         : gvalign == "axis" ? - (gh/2) + as_length ("1yfrac")
	 : - gh / 2;
  if (is_tuple (t, "scale", 2) && is_func (t[2], TUPLE, 2)) {
    SI magn= as_length (t[1]);
    SI x   = as_length (t[2][0]);
    SI y   = as_length (t[2][1]);
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
    clip_lim1= fr [point (p0[0], p0[1] - gh)];
    clip_lim2= fr [point (p0[0] + gw, p0[1])];
  }
  else if (gvalign == "bottom") {
    clip_lim1= fr [point (p0[0], p0[1])];
    clip_lim2= fr [point (p0[0] + gw, p0[1] + gh)];
  }
  else if (gvalign == "axis") {
    clip_lim1= fr [point (p0[0], p0[1] - (gh/2) + as_length ("1yfrac"))];
    clip_lim2= fr [point (p0[0] + gw, p0[1] + (gh/2) + as_length ("1yfrac"))];
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
  dash_style= array<bool> (0);
  dash_motif= array<point> (0);
  if (is_string (t)) {
    string s= as_string (t);
    if (N(s) > 0 && (s[0] == '0' || s[0] == '1')) {
      int i, n= N(s);
      dash_style= array<bool> (n);
      for (i=0; i<n; i++)
        dash_style[i]= (s[i] != '0');
    }
    else if (N(s) > 0) {
      if (s == "zigzag")
        dash_motif << point (0.25, 0.5) << point (0.75, -0.5);
      else if (s == "wave")
        for (int k=1; k<=11; k++)
          dash_motif << point (k / 12.0, sin (3.141592*k / 6.0) / 2.0);
      else if (s == "pulse")
        dash_motif << point (0.0, 0.5) << point (0.5, 0.5)
                   << point (0.5, -0.5) << point (1.0, -0.5);
      else if (s == "loops")
        for (int k=1; k<=11; k++) {
          double c= (1.0 - cos (3.141592*k / 6.0)) / 2.0;
          double s= sin (3.141592*k / 6.0) / 2.0;
          dash_motif << point (k / 12.0 + c, s);
        }
      else if (s == "meander") {
        double u= 1.0 / 12.0;
        dash_motif << point (-0.5*u, 0.0)
                   << point (5*u, 0.0) << point (5*u, 5*u)
                   << point (u, 5*u) << point (u, 2*u)
                   << point (3*u, 2*u) << point (3*u, 3*u)
                   << point (2*u, 3*u) << point (2*u, 4*u)
                   << point (4*u, 4*u) << point (4*u, u)
                   << point (0.0, u) << point (0.0, 6*u)
                   << point (5.5*u, 6*u)
                   << point (11*u, 6*u) << point (11*u, u)
                   << point (7*u, u) << point (7*u, 4*u)
                   << point (9*u, 4*u) << point (9*u, 3*u)
                   << point (8*u, 3*u) << point (8*u, 2*u)
                   << point (10*u, 2*u) << point (10*u, 5*u)
                   << point (6*u, 5*u) << point (6*u, 0.0);
        for (int i=0; i<N(dash_motif); i++)
          dash_motif[i][0] += 0.5*u;
      }
      for (int i=0; i<N(dash_motif); i++)
        dash_motif[i][1] *= dash_style_ratio;
    }
  }
}

void
edit_env_rep::update_dash_style_unit () {
  tree t= read (DASH_STYLE_UNIT);
  if (is_tuple (t) && N(t) == 2) {
    SI hunit= as_length (t[0]);
    SI vunit= as_length (t[1]);
    dash_style_unit = hunit;
    dash_style_ratio= ((double) vunit) / max (((double) hunit), 1.0);
  }
  else {
    dash_style_unit= as_length (t);
    dash_style_ratio= 1.0;
  }
  if (N(dash_motif) != 0) update_dash_style ();
}

void
decompose_length (string s, double& x, string& un) {
  int i;
  for (i=0; i<N(s); i++)
    if (is_locase (s[i])) break;
  x = as_double (s (0, i));
  un= s (i, N(s));
}

tree
edit_env_rep::decode_arrow (tree t, string l, string h) {
  if (is_string (t)) {
    string s= t->label;
    if (s == "" || s == "none") return "";
    double lx, hx;
    string lun, hun;
    decompose_length (l, lx, lun);
    decompose_length (h, hx, hun);
    if (s == "<less>")
      return tree (LINE,
                   tree (_POINT, l, h),
                   tree (_POINT, "0" * lun, "0" * hun),
                   tree (_POINT, l, as_string (-hx) * hun));
    if (s == "<gtr>")
      return tree (LINE,
                   tree (_POINT, as_string (-lx) * lun, h),
                   tree (_POINT, "0" * lun, "0" * hun),
                   tree (_POINT, as_string (-lx) * lun,
                                 as_string (-hx) * hun));
    if (s == "<less>|")
      return tree (WITH, FILL_COLOR, tree (VALUE, COLOR),
                   LINE_WIDTH, "0ln",
                   tree (CLINE,
                         tree (_POINT, l, h),
                         tree (_POINT, "0" * lun, "0" * hun),
                         tree (_POINT, l, as_string (-hx) * hun)));
    if (s == "|<gtr>")
      return tree (WITH, FILL_COLOR, tree (VALUE, COLOR),
                   LINE_WIDTH, "0ln",
                   tree (CLINE,
                         tree (_POINT, as_string (-lx) * lun, h),
                         tree (_POINT, "0" * lun, "0" * hun),
                         tree (_POINT, as_string (-lx) * lun,
                               as_string (-hx) * hun)));
    if (s == "<gtr>")
      return tree (LINE,
                   tree (_POINT, as_string (-lx) * lun, h),
                   tree (_POINT, "0" * lun, "0" * hun),
                   tree (_POINT, as_string (-lx) * lun,
                                 as_string (-hx) * hun));
    if (s == "<less><less>")
      return tree (GR_GROUP,
                   tree (LINE,
                         tree (_POINT, l, h),
                         tree (_POINT, "0" * lun, "0" * hun),
                         tree (_POINT, l, as_string (-hx) * hun)),
                   tree (LINE,
                         tree (_POINT, as_string (0 * lx) * lun, h),
                         tree (_POINT, as_string (-lx) * lun, "0" * hun),
                         tree (_POINT, as_string (0 * lx) * lun,
                               as_string (-hx) * hun)));
    if (s == "<gtr><gtr>")
      return tree (GR_GROUP,
                   tree (LINE,
                         tree (_POINT, as_string (-lx) * lun, h),
                         tree (_POINT, "0" * lun, "0" * hun),
                         tree (_POINT, as_string (-lx) * lun,
                               as_string (-hx) * hun)),
                   tree (LINE,
                         tree (_POINT, as_string (0 * lx) * lun, h),
                         tree (_POINT, as_string (lx) * lun, "0" * hun),
                         tree (_POINT, as_string (0 * lx) * lun,
                               as_string (-hx) * hun)));                   
    if (s == "|")
      return tree (LINE,
                   tree (_POINT, "0" * lun, h),
                   tree (_POINT, "0" * lun, as_string (-hx) * hun));
    if (s == "o")
      return tree (WITH, FILL_COLOR, tree (VALUE, COLOR),
                   LINE_WIDTH, "0ln",
                   tree (CARC,
                         tree (_POINT, "0" * lun, h),
                         tree (_POINT, l, "0" * hun),
                         tree (_POINT, "0" * lun, as_string (-hx) * hun)));
    return "";
  }
  else return t;
}

void
edit_env_rep::update_line_arrows () {
  line_arrows= array<tree> (2);
  string l= get_string (ARROW_LENGTH);
  string h= get_string (ARROW_HEIGHT);
  line_arrows[0]= decode_arrow (env [ARROW_BEGIN], l, h);
  line_arrows[1]= decode_arrow (env [ARROW_END], l, h);
  if (line_arrows[0] != "")
    line_arrows[0]= tree (WITH, LINE_PORTION, "1", line_arrows[0]);
  if (line_arrows[1] != "")
    line_arrows[1]= tree (WITH, LINE_PORTION, "1", line_arrows[1]);
}

void
edit_env_rep::update () {
  zoomf          = normal_zoom (get_double (ZOOM_FACTOR));
  pixel          = (SI) tm_round ((std_shrinkf * PIXEL) / zoomf);
  magn           = get_double (MAGNIFICATION);
  index_level    = get_int (MATH_LEVEL);
  display_style  = get_bool (MATH_DISPLAY);
  math_condensed = get_bool (MATH_CONDENSED);
  vert_pos       = get_int (MATH_VPOS);
  nesting_level  = get_int (MATH_NESTING_LEVEL);
  preamble       = get_bool (PREAMBLE);
  spacing_policy = get_spacing_id (env[SPACING_POLICY]);
  math_font_sizes= env[MATH_FONT_SIZES];
  size_cache     = array<array<int> > ();

  update_mode ();
  update_info_level ();
  update_language ();
  update_font ();

  update_geometry ();
  update_frame ();
  point_style = get_string (POINT_STYLE);
  point_size  = get_length (POINT_SIZE);
  point_border= get_length (POINT_BORDER);
  update_color ();
  update_pattern_mode ();
  update_dash_style ();
  update_dash_style_unit ();
  update_line_arrows ();
  line_portion= get_double (LINE_PORTION);
  text_at_halign= get_string (TEXT_AT_HALIGN);
  text_at_valign= get_string (TEXT_AT_VALIGN);
  doc_at_valign= get_string (DOC_AT_VALIGN);

  update_src_style ();
  update_src_special ();
  update_src_compact ();
  update_src_close ();
  src_tag_color= get_string (SRC_TAG_COLOR);
  src_tag_col= named_color (src_tag_color);

  frac_max   = get_length (MATH_FRAC_LIMIT);
  table_max  = get_length (MATH_TABLE_LIMIT);
  flatten_pen= pencil (env[MATH_FLATTEN_COLOR], alpha, get_length (LINE_WIDTH));
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
  case Env_Zoom:
    zoomf= normal_zoom (get_double (ZOOM_FACTOR));
    pixel= (SI) tm_round ((std_shrinkf * PIXEL) / zoomf);
    break;
  case Env_Magnification:
    magn= get_double (MAGNIFICATION);
    update_font ();
    update_color ();
    update_dash_style_unit ();
    break;
  case Env_Magnify:
    mgfy= get_double (MAGNIFY);
    update_font ();
    update_color ();
    update_dash_style_unit ();
    break;
  case Env_Language:
    update_language ();
    break;
  case Env_Mode:
    update_mode ();
    update_language ();
    update_font ();
    break;
  case Env_Info_Level:
    update_info_level ();
    break;
  case Env_Font:
    update_font ();
    break;
  case Env_Font_Size:
    update_font ();
    break;
  case Env_Font_Sizes:
    math_font_sizes= env[MATH_FONT_SIZES];
    size_cache= array<array<int> > ();
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
  case Env_Math_Nesting:
    nesting_level= get_int (MATH_NESTING_LEVEL);
    break;
  case Env_Math_Width:
    frac_max= get_length (MATH_FRAC_LIMIT);
    table_max= get_length (MATH_TABLE_LIMIT);
    flatten_pen= pencil (env[MATH_FLATTEN_COLOR], alpha, get_length (LINE_WIDTH));
    break;
  case Env_Color:
    update_color ();
    break;
  case Env_Pattern_Mode:
    update_pattern_mode ();
    break;
  case Env_Spacing:
    spacing_policy= get_spacing_id (env[SPACING_POLICY]);
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
  case Env_Point_Size:
    point_size= get_length (POINT_SIZE);
    point_border= get_length (POINT_BORDER);
    break;
  case Env_Line_Width:
    update_color ();
    break;
  case Env_Dash_Style:
    update_dash_style();
    break;
  case Env_Dash_Style_Unit:
    update_dash_style_unit ();
    break;
  case Env_Fill_Color:
    update_color ();
    break;
  case Env_Line_Arrows:
    update_line_arrows();
    break;
  case Env_Line_Portion:
    line_portion= get_double (LINE_PORTION);
    break;
  case Env_Text_At_Halign:
    text_at_halign= get_string (TEXT_AT_HALIGN);
    break;
  case Env_Text_At_Valign:
    text_at_valign= get_string (TEXT_AT_VALIGN);
    break;
  case Env_Doc_At_Valign:
    doc_at_valign= get_string (DOC_AT_VALIGN);
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
  case Env_Src_Color:
    src_tag_color= get_string (SRC_TAG_COLOR);
    src_tag_col= named_color (src_tag_color);
    break;
  }
}
