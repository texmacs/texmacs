
/******************************************************************************
* MODULE     : env_default.cpp
* DESCRIPTION: default values of the environment variables
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "env.hpp"

/*static*/ hashmap<string,tree> default_env (UNINIT);

/*static*/ void
initialize_default_env () {
  if (N(default_env) != 0) return;
  hashmap<string,tree>& env= default_env;

  tree identity_m (MACRO, "x", tree (ARG, "x"));
  tree tabular_m (MACRO, "x", tree (TFORMAT, tree (ARG, "x")));
  tree the_page (MACRO, compound ("page-nr"));

  tree gr_geometry (TUPLE, "geometry", "1par", "0.6par", "center");
  tree gr_frame (TUPLE, "scale", "1cm", tree (TUPLE, "0.5gw", "0.5gh"));

  tree gr_grid ("");
  tree gr_edit_grid ("");
  tree gr_grid_aspect (TUPLE,
		       tuple ("axes", "#808080"),
		       tuple ("1", "#c0c0c0"),
		       tuple ("10", "#e0e0ff"));

  env (DPI)              = "600";       // resolution in dots per inch
  env (SFACTOR)          = "5";         // shrinking factor on screen
  env (PREAMBLE)         = "false";     // preamble mode ?
  env (SAVE_AUX)         = "true";      // save auxiliary data on disk ?
  env (MODE)             = "text";      // typesetting mode
  env (INFO_FLAG)        = "minimal";   // information about labels, etc.
  env (WINDOW_BARS)      = "auto";      // override menu/icon bar settings
  env (SCROLL_BARS)      = "true";      // allow scroll bars around canvas?
  env (IDENTITY)         = identity_m;  // identity macro
  env (TABULAR)          = tabular_m;   // tabular macro
  env (THE_LABEL)        = "?";         // value of the next label
  env (THE_TAGS)         = tree(TUPLE); // current tags
  env (THE_MODULES)      = tree(TUPLE); // necessary modules and plug-ins

  env (FONT)             = "roman";     // the font name in text mode
  env (FONT_FAMILY)      = "rm";        // the font family in text mode
  env (FONT_SERIES)      = "medium";    // the font series in text mode
  env (FONT_SHAPE)       = "right";     // the font shape in text mode
  env (FONT_SIZE)        = "1";         // the font size multiplier
  env (FONT_BASE_SIZE)   = "10";        // the font base size
  env (MAGNIFICATION)    = "1";         // magnification (slides for instance)
  env (COLOR)            = "black";     // the color
  env (BG_COLOR)         = "white";     // the background color
  env (LOCUS_COLOR)      = "global";    // the color of loci
  env (VISITED_COLOR)    = "global";    // the color of visited loci
  env (LANGUAGE)         = "english";   // the language
  env (ATOM_DECORATIONS) = DATOMS;      // dots, underline, hyperlinks?, etc.
  env (LINE_DECORATIONS) = DLINES;      // boxed pars, nested envs, etc.
  env (PAGE_DECORATIONS) = DPAGES;      // future headers, footers, etc.
  env (XOFF_DECORATIONS) = "0tmpt";     // hor. placement of decorations
  env (YOFF_DECORATIONS) = "0tmpt";     // vert. placement of decorations

  env (MATH_LANGUAGE)    = "std-math";  // the default mathematical language
  env (MATH_FONT)        = "roman";     // the font name in math mode
  env (MATH_FONT_FAMILY) = "mr";        // the font family in math mode
  env (MATH_FONT_SERIES) = "medium";    // the font series in math mode
  env (MATH_FONT_SHAPE)  = "normal";    // the font shape in math mode
  env (MATH_LEVEL)       = "0";         // the index level (0, 1 or 2)
  env (MATH_DISPLAY)     = "false";     // true if we are in display style
  env (MATH_CONDENSED)   = "false";     // ignore spaces between operators ?
  env (MATH_VPOS)        = "0";         // used in fractions (-1, 0 or 1)
  env (MATH_NESTING_MODE)= "off";       // color nested brackets?
  env (MATH_NESTING_LEVEL)= "0";        // nesting level inside brackets

  env (PROG_LANGUAGE)    = "scheme";    // the default programming language
  env (PROG_SCRIPTS)     = "scheme";    // the scripting language
  env (PROG_FONT)        = "roman";     // the font name in prog mode
  env (PROG_FONT_FAMILY) = "tt";        // the font family in prog mode
  env (PROG_FONT_SERIES) = "medium";    // the font series in prog mode
  env (PROG_FONT_SHAPE)  = "right";     // the font shape in prog mode
  env (PROG_SESSION)     = "default";   // computer algebra session name

  env (PAR_MODE)         = "justify";   // outline method
  env (PAR_FLEXIBILITY)  = "1000.0";    // threshold for switching to ragged
  env (PAR_HYPHEN)       = "professional"; // quality of hyphenation
  env (PAR_WIDTH)        = "auto";      // width of paragraph
  env (PAR_LEFT)         = "0cm";       // left indentation
  env (PAR_RIGHT)        = "0cm";       // right indentation
  env (PAR_FIRST)        = "1.5fn";     // extra first indentation
  env (PAR_NO_FIRST)     = "false";     // no extra first indent. on next line
  env (PAR_SEP)          = "0.2fn";     // extra space between paragraph lines
  env (PAR_HOR_SEP)      = "0.5fn";     // min. hor. spc. between ink for shove
  env (PAR_VER_SEP)      = "0.2fn";     // min. ver. spc. between ink
  env (PAR_LINE_SEP)     = "0.025fns";  // extra (small) space between lines
  env (PAR_PAR_SEP)      = "0.5fns";    // extra space between paragraphs
  env (PAR_FNOTE_SEP)    = "0.2fn";     // min space between diff footnotes
  env (PAR_COLUMNS)      = "1";         // number of columns
  env (PAR_COLUMNS_SEP)  = "2fn";       // separation between columns

  env (PAGE_MEDIUM)      = "papyrus";   // paper medium: paper, papyrus, auto
  env (PAGE_PRINTED)     = "false";     // printed version?
  env (PAGE_TYPE)        = "a4";        // paper type (-> width & height)
  env (PAGE_ORIENTATION) = "portrait";  // paper orientation
  env (PAGE_WIDTH_MARGIN)  = "false";   // compute margins from par-width?
  env (PAGE_HEIGHT_MARGIN) = "false";   // compute margins from par-width?
  env (PAGE_SCREEN_MARGIN) = "true";    // special margins for screen editing?
  env (PAGE_BREAKING)    = "optimal";   // quality of page breaking
  env (PAGE_FLEXIBILITY) = "1.0";       // flexibility factor of stretch
  env (PAGE_NR)          = "0";         // the page number
  env (PAGE_THE_PAGE)    = the_page;    // the page number as text
  env (PAGE_WIDTH)       = "auto";      // physical width of pages
  env (PAGE_HEIGHT)      = "auto";      // physical height of pages
  env (PAGE_ODD)         = "auto";      // left margin on odd pages
  env (PAGE_EVEN)        = "auto";      // left margin on even pages
  env (PAGE_RIGHT)       = "auto";      // right margin in auto mode
  env (PAGE_TOP)         = "auto";      // top margin
  env (PAGE_BOT)         = "auto";      // bottom margin
  env (PAGE_USER_HEIGHT) = "522pt";     // height of principal text
  env (PAGE_ODD_SHIFT)   = "0mm";       // odd page marginal shift wrt center
  env (PAGE_EVEN_SHIFT)  = "0mm";       // even page marginal shift wrt center
  env (PAGE_SHRINK)      = "1fn";       // emergency page length shrinking
  env (PAGE_EXTEND)      = "0fn";       // emergency page length extension
  env (PAGE_HEAD_SEP)    = "8mm";       // separation between header and text
  env (PAGE_FOOT_SEP)    = "8mm";       // separation between footer and text
  env (PAGE_ODD_HEADER)  = "";          // header on odd pages
  env (PAGE_ODD_FOOTER)  = "";          // footer on odd pages
  env (PAGE_EVEN_HEADER) = "";          // header on even pages
  env (PAGE_EVEN_FOOTER) = "";          // footer on even pages
  env (PAGE_SCREEN_WIDTH)  = "10cm";    // width of current window (for auto)
  env (PAGE_SCREEN_HEIGHT) = "10cm";    // height of current window (for auto)
  env (PAGE_SCREEN_LEFT) = "5mm";       // left margin for screen editing
  env (PAGE_SCREEN_RIGHT)= "5mm";       // right margin for screen editing
  env (PAGE_SCREEN_TOP)  = "15mm";      // top margin for screen editing
  env (PAGE_SCREEN_BOT)  = "15mm";      // bottom margin for screen editing
  env (PAGE_SHOW_HF)     = "false";     // show header and footer
  env (PAGE_FNOTE_SEP)   = "1.0fn";     // space between text & footnotes
  env (PAGE_FNOTE_BARLEN)= "7.5fn";     // length of footnote separating bar
  env (PAGE_FLOAT_SEP)   = "1.5fn";     // space between text & floats
  env (PAGE_MNOTE_SEP)   = "5mm";       // space between text & marginal notes
  env (PAGE_MNOTE_WIDTH) = "15mm";      // width of marginal notes

  env (TABLE_WIDTH)      = "";          // width of table
  env (TABLE_HEIGHT)     = "";          // height of table
  env (TABLE_HMODE)      = "auto";      // width determination mode
  env (TABLE_VMODE)      = "auto";      // height determination mode
  env (TABLE_HALIGN)     = "l";         // horizontal alignment
  env (TABLE_VALIGN)     = "f";         // vertical alignment (fraction height)
  env (TABLE_ROW_ORIGIN) = "0";         // row origin
  env (TABLE_COL_ORIGIN) = "0";         // column origin
  env (TABLE_LSEP)       = "0fn";       // left padding around table
  env (TABLE_RSEP)       = "0fn";       // right padding around table
  env (TABLE_BSEP)       = "0fn";       // bottom padding around table
  env (TABLE_TSEP)       = "0fn";       // top padding around table
  env (TABLE_LBORDER)    = "0ln";       // left table border width
  env (TABLE_RBORDER)    = "0ln";       // right table border width
  env (TABLE_BBORDER)    = "0ln";       // bottom table border width
  env (TABLE_TBORDER)    = "0ln";       // top table border width
  env (TABLE_HYPHEN)     = "n";         // vertical hyphenation
  env (TABLE_MIN_ROWS)   = "";          // suggested minimal number of rows
  env (TABLE_MIN_COLS)   = "";          // suggested minimal number of columns
  env (TABLE_MAX_ROWS)   = "";          // suggested maximal number of rows
  env (TABLE_MAX_COLS)   = "";          // suggested maximal number of columns

  env (CELL_DECORATION)  = "";          // decorating table of cell
  env (CELL_FORMAT)      = TFORMAT;     // format of cell
  env (CELL_BACKGROUND)  = "";          // background color of cell
  env (CELL_ORIENTATION) = "portrait";  // orientation of cell  
  env (CELL_WIDTH)       = "";          // width of cell
  env (CELL_HEIGHT)      = "";          // height of cell
  env (CELL_HPART)       = "";          // take part of unused horizontal space
  env (CELL_VPART)       = "";          // take part of unused vertical space
  env (CELL_HMODE)       = "auto";      // width determination mode
  env (CELL_VMODE)       = "auto";      // height determination mode
  env (CELL_HALIGN)      = "l";         // horizontal alignment
  env (CELL_VALIGN)      = "B";         // vertical alignment
  env (CELL_LSEP)        = "0fn";       // left cell padding
  env (CELL_RSEP)        = "0fn";       // right cell padding
  env (CELL_BSEP)        = "0fn";       // bottom cell padding
  env (CELL_TSEP)        = "0fn";       // top cell padding
  env (CELL_LBORDER)     = "0ln";       // left cell border width
  env (CELL_RBORDER)     = "0ln";       // right cell border width
  env (CELL_BBORDER)     = "0ln";       // bottom cell border width
  env (CELL_TBORDER)     = "0ln";       // top cell border width
  env (CELL_VCORRECT)    = "a";         // vertical limits correction
  env (CELL_HYPHEN)      = "n";         // horizontal hyphenation
  env (CELL_BLOCK)       = "auto";      // cell contains block content?
  env (CELL_ROW_SPAN)    = "1";         // row span of cell
  env (CELL_COL_SPAN)    = "1";         // column span of cell
  env (CELL_ROW_NR)      = "1";         // row coordinate of cell
  env (CELL_COL_NR)      = "1";         // column coordinate of cell

  env (POINT_STYLE)      = "disk";      // point style (square, circle...)

  env (LINE_WIDTH)       = "1ln";       // line width in graphics
  env (DASH_STYLE)       = "none";      // dash style
  env (DASH_STYLE_UNIT)  = "5ln";       // dash style unit
  env (LINE_ARROWS)      = "none";      // arrows at end of lines
  env (LINE_CAPS)        = "normal";    // junctions in multilines
  env (FILL_COLOR)       = "none";      // fill color
  env (FILL_STYLE)       = "plain";     // fill style
  env (TEXTAT_HALIGN)    = "left";      // horizontal text-at alignment
  env (TEXTAT_VALIGN)    = "base";      // vertical text-at alignment

  env (GR_GEOMETRY)      = gr_geometry; // geometry of graphics
  env (GR_FRAME)         = gr_frame;    // coordinate frame for graphics
  env (GR_MODE)          = "line";      // graphical mode
  env (GR_COLOR)         = "default";   // color of new objects
  env (GR_POINT_STYLE)   = "default";   // point style of new objects
  env (GR_LINE_WIDTH)    = "default";   // line width for new objects
  env (GR_DASH_STYLE)      = "default"; // dash style for new objects
  env (GR_DASH_STYLE_UNIT) = "default"; // dash style unit for new objects
  env (GR_LINE_ARROWS)   = "default";   // line arrows for new objects
  env (GR_FILL_COLOR)    = "default";   // fill color for new objects
  env (GR_TEXTAT_HALIGN) = "default";   // horiz. alignment for new text-ats
  env (GR_TEXTAT_VALIGN) = "default";   // vert. alignment for new text-ats
  env (GR_GRID)             = gr_grid;        // grid for graphics
  env (GR_GRID_ASPECT)      = gr_grid_aspect; // grid aspect
  env (GR_EDIT_GRID)        = gr_edit_grid;   // edit grid
  env (GR_EDIT_GRID_ASPECT) = gr_grid_aspect; // edit grid (subdivisions)

  env (SRC_STYLE)        = "angular";   // style for "source" tags
  env (SRC_SPECIAL)      = "normal";    // special treatment of certain tags
  env (SRC_COMPACT)      = "normal";    // compact inline/multi-paragraph tags?
  env (SRC_CLOSE)        = "compact";   // how to close long tags

  env (CANVAS_TYPE)           = "plain";        // which kind of scrollbars
  env (CANVAS_COLOR)          = "white";        // canvas colour
  env (CANVAS_HPADDING)       = "0px";          // horizontal canvas padding
  env (CANVAS_VPADDING)       = "0px";          // vertical canvas padding
  env (CANVAS_BAR_WIDTH)      = "1em";          // width of scroll bars
  env (CANVAS_BAR_PADDING)    = "0.25em";       // distance of scrollbars
  env (CANVAS_BAR_COLOR)      = "grey";         // color of bar
  env (ORNAMENT_SHAPE)        = "rectangular";  // shape of the ornament
  env (ORNAMENT_BORDER)       = "1ln";          // border width
  env (ORNAMENT_HPADDING)     = "0px";          // horizontal padding of body
  env (ORNAMENT_VPADDING)     = "0px";          // vertical padding of body
  env (ORNAMENT_COLOR)        = "light grey";   // background color
  env (ORNAMENT_SUNNY_COLOR)  = "white";        // sunny color
  env (ORNAMENT_SHADOW_COLOR) = "dark grey";    // shadow color

  /* hiding and showing content */
  env ("shown")= identity_m;
  env ("ignore")=
    tree (MACRO, "body", tree (HIDDEN, tree (ARG, "body")));

  /* linking macros */
  tree src_id (ID, tree (HARD_ID, tree (ARG, "body")));
  tree ref_id (ID, tree (HARD_ID, tree (ARG, "Id")));
  tree dest_url (URL, tree (ARG, "destination"));
  tree dest_script (SCRIPT, tree (ARG, "destination"), tree (ARG, "where"));
  tree dest_ref (URL, tree (MERGE, "#", tree (ARG, "Id")));
  tree anchor (ID, tree (MERGE, "#", tree (ARG, "Id")));
  tree ln1 (LINK, "hyperlink", copy (src_id), copy (dest_url));
  tree ln2 (LINK, "action", copy (src_id), copy (dest_script));
  tree ln3 (LINK, "hyperlink", copy (ref_id), copy (dest_ref));
  tree ln4 (LINK, "anchor", anchor);
  tree labflag (FLAG, tree (ARG, "Id"), "blue", "Id");
  tree labtxt (SET_BINDING, tree (ARG, "Id"), tree (VALUE, THE_LABEL));
  tree merged (MERGE, tree (VALUE, THE_TAGS), tuple (tree (ARG, "Id")));
  tree tagflag (FLAG, tree (ARG, "Id"), "blue", "Id");
  tree reftxt (GET_BINDING, tree (ARG, "Id"));
  tree preftxt (GET_BINDING, tree (ARG, "Id"), "1");
  env ("hlink")= tree (MACRO, "body", "destination",
		       tree (LOCUS, copy (src_id), ln1,
                             tree (ARG, "body")));
  env ("action")= tree (MACRO, "body", "destination", "where",
			tree (LOCUS, copy (src_id), ln2,
                              tree (ARG, "body")));
  env ("label")= tree (MACRO, "Id", 
		       tree (LOCUS, copy (ref_id), ln4, tree (CONCAT, labflag, labtxt)));
  env ("tag")= tree (MACRO, "Id", "body",
		     tree (WITH, "the-tags", merged,
			   tree (SURROUND, tagflag, "",
                                 tree (ARG, "body"))));
  env ("reference")= tree (MACRO, "Id",
			   tree (LOCUS, copy (ref_id), ln3, reftxt));
  env ("pageref")= tree (MACRO, "Id",
			 tree (LOCUS, copy (ref_id), copy (ln3), preftxt));

  /* further standard macros */
  env ("error")=
    tree (MACRO, "message",
          tree (REWRITE_INACTIVE, tree (ARG, "message"), "error"));
  env ("style-only")=
    tree (MACRO, "body", tree (ARG, "body"));
  env ("style-only*")=
    tree (MACRO, "body", tree (ARG, "body"));
  env ("active")=
    tree (MACRO, "body", tree (ARG, "body"));
  env ("active*")=
    tree (MACRO, "body", tree (ARG, "body"));
  env ("inactive")=
    tree (MACRO, "body",
          tree (REWRITE_INACTIVE, tree (ARG, "body"), "once"));
  env ("inactive*")=
    tree (MACRO, "body",
          tree (REWRITE_INACTIVE, tree (ARG, "body"), "recurse"));
  env ("right-flush")=
    tree (MACRO, tree (HTAB, "0fn", "first"));
  env ("indent*")=
    tree (MACRO, "body",
	  tree (WITH, PAR_LEFT, tree (PLUS, tree (VALUE, PAR_LEFT), "1.5fn"),
		tree (ARG, "body")));
  env ("indent")=
    tree (MACRO, "body",
	  tree (SURROUND, "", compound ("right-flush"),
		compound ("indent*", tree (ARG, "body"))));
  env ("math")=
    tree (MACRO, "body", tree (WITH, MODE, "math", tree (ARG, "body")));
  env ("text")=
    tree (MACRO, "body", tree (WITH, MODE, "text", tree (ARG, "body")));
  env ("pre-edit")=
    tree (MACRO, "body", tree (WITH, COLOR, "#4040c0", tree (ARG, "body")));
  env ("mutator")=
    tree (MACRO, "body", "y", tree (ARG, "body"));

  /* syntactic highlighting */
  env ("src-regular")=
    tree (MACRO, "body", tree (WITH, COLOR, "black", tree (ARG, "body")));
  env ("src-macro")=
    tree (MACRO, "body", tree (WITH, MODE, "src", COLOR, "blue",
                               FONT_FAMILY, "ss", tree (ARG, "body")));
  env ("src-var")=
    tree (MACRO, "body", tree (WITH, MODE, "src", COLOR, "dark green",
                               FONT_SHAPE, "italic", tree (ARG, "body")));
  env ("src-arg")=
    tree (MACRO, "body", tree (WITH, MODE, "src", COLOR, "brown",
                               FONT_SHAPE, "italic", tree (ARG, "body")));
  env ("src-tt")=
    tree (MACRO, "body", tree (WITH, MODE, "src", COLOR, "#228",
                               FONT_FAMILY, "tt", tree (ARG, "body")));
  env ("src-numeric")=
    tree (MACRO, "body", tree (WITH, MODE, "src", COLOR, "#848",
                               tree (ARG, "body")));
  env ("src-textual")=
    tree (MACRO, "body", tree (WITH, MODE, "src", COLOR, "black",
                               tree (ARG, "body")));
  env ("src-length")=
    tree (MACRO, "body", tree (WITH, MODE, "src", COLOR, "#288",
                               tree (ARG, "body")));
  env ("src-unknown")=
    tree (MACRO, "body", tree (WITH, COLOR, "#C68", tree (ARG, "body")));
  env ("src-error")=
    tree (MACRO, "body", tree (WITH, COLOR, "red", tree (ARG, "body")));

  /* for correct importation of style files and packages */
  env ("src-title")= identity_m;
  env ("src-style-file")=
    tree (MACRO, "style", "version",
	  tree (ASSIGN,
		tree (MERGE, tree (ARG, "style"), "-style"),
		tree (ARG, "version")));
  env ("src-package")=
    tree (MACRO, "package", "version",
	  tree (CONCAT,
		tree (ASSIGN,
		      tree (MERGE, tree (ARG, "package"), "-package"),
		      tree (ARG, "version")),
		tree (ASSIGN,
		      tree (MERGE, tree (ARG, "package"), "-dtd"),
		      tree (ARG, "version"))));
  env ("src-package-dtd")=
    tree (MACRO, "package", "version", "drd", "drd-version",
	  tree (CONCAT,
		tree (ASSIGN,
		      tree (MERGE, tree (ARG, "package"), "-package"),
		      tree (ARG, "version")),
		tree (ASSIGN,
		      tree (MERGE, tree (ARG, "drd"), "-dtd"),
		      tree (ARG, "drd-version"))));
}
