
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

  tree gr_transf (TUPLE,
                  tuple ("1.0", "0.0", "0.0", "0.0"),
                  tuple ("0.0", "1.0", "0.0", "0.0"),
                  tuple ("0.0", "0.0", "1.0", "0.0"),
                  tuple ("0.0", "0.0", "0.0", "1.0"));         

  env (DPI)              = "600";       // resolution in dots per inch
  env (ZOOM_FACTOR)      = "1";         // zoom factor on screen
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
  env (WARN_MISSING)     = "true";      // warn about missing references
  env (GLOBAL_TITLE)     = "";          // global document title
  env (GLOBAL_AUTHOR)    = "";          // global document author
  env (GLOBAL_SUBJECT)   = "";          // global document subject
  env (LENGTH_MODE)      = "magnified"; // length units under magnification

  env (FONT)             = "roman";     // the font name in text mode
  env (FONT_FAMILY)      = "rm";        // the font family in text mode
  env (FONT_SERIES)      = "medium";    // the font series in text mode
  env (FONT_SHAPE)       = "right";     // the font shape in text mode
  env (FONT_SIZE)        = "1";         // the font size multiplier
  env (FONT_BASE_SIZE)   = "10";        // the font base size
  env (FONT_EFFECTS)     = "";          // additional effects applied to font
  env (MAGNIFICATION)    = "1";         // magnification (slides for instance)
  env (COLOR)            = "black";     // the color
  env (OPACITY)          = "100%";      // the opacity
  env (BG_COLOR)         = "white";     // the background color
  env (LOCUS_COLOR)      = "global";    // the color of loci
  env (VISITED_COLOR)    = "global";    // the color of visited loci
  env (NO_PATTERNS)      = "false";     // disable background patterns
  env (LANGUAGE)         = "english";   // the language
  env (SPACING_POLICY)   = "default";   // the spacing policy
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
  env (MATH_FONT_SIZES)  = "default";   // various script sizes
  env (MATH_LEVEL)       = "0";         // the index level (0, 1 or 2)
  env (MATH_DISPLAY)     = "false";     // true if we are in display style
  env (MATH_CONDENSED)   = "false";     // ignore spaces between operators ?
  env (MATH_VPOS)        = "0";         // used in fractions (-1, 0 or 1)
  env (MATH_NESTING_MODE)= "off";       // color nested brackets?
  env (MATH_NESTING_LEVEL)= "0";        // nesting level inside brackets
  env (MATH_FRAC_LIMIT)  = "100par";    // maximal width of 2D fraction
  env (MATH_TABLE_LIMIT) = "100par";    // maximal width of 2D math table
  env (MATH_FLATTEN_COLOR)  = "#448";   // bracket color when 2D width excess
  env (MATH_TOP_SWELL_START)= "1.7ex";  // start padding above this level
  env (MATH_TOP_SWELL_END)  = "3.5ex";  // maximal padding reached here
  env (MATH_BOT_SWELL_START)= "-0.7ex"; // start padding below this level
  env (MATH_BOT_SWELL_END)  = "-2.5ex"; // maximal padding reached here

  env (PROG_LANGUAGE)    = "scheme";    // the default programming language
  env (PROG_SCRIPTS)     = "none";      // the scripting language
  env (PROG_FONT)        = "roman";     // the font name in prog mode
  env (PROG_FONT_FAMILY) = "tt";        // the font family in prog mode
  env (PROG_FONT_SERIES) = "medium";    // the font series in prog mode
  env (PROG_FONT_SHAPE)  = "right";     // the font shape in prog mode
  env (PROG_SESSION)     = "default";   // computer algebra session name

  env (PAR_MODE)         = "justify";   // outline method
  env (PAR_FLEXIBILITY)  = "1000";      // threshold for switching to ragged
  env (PAR_HYPHEN)       = "professional"; // quality of hyphenation
  env (PAR_MIN_PENALTY)  = "0";         // minimal page break penalty
  env (PAR_SPACING)      = "plain";     // spacing mode (for CJK)
  env (PAR_KERNING_REDUCE) = "auto";    // reduced kerning around characters
  env (PAR_KERNING_STRETCH)= "auto";    // extra kerning around characters
  env (PAR_KERNING_MARGIN) = "false";   // use marginal kerning (protrusion)
  env (PAR_CONTRACTION)  = "auto";      // maximal glyph contraction
  env (PAR_EXPANSION)    = "auto";      // maximal glyph expansion
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
  env (PAR_SWELL)        = "0ex";       // extra padding around large lines

  env (PAGE_MEDIUM)      = "papyrus";   // paper medium: paper, papyrus, auto
  env (PAGE_PRINTED)     = "false";     // printed version?
  env (PAGE_TYPE)        = "a4";        // paper type (-> width & height)
  env (PAGE_ORIENTATION) = "portrait";  // paper orientation
  env (PAGE_CROP_MARKS)  = "";          // paper size on which we print
  env (PAGE_WIDTH_MARGIN)  = "false";   // compute margins from par-width?
  env (PAGE_HEIGHT_MARGIN) = "false";   // compute margins from par-width?
  env (PAGE_SCREEN_MARGIN) = "true";    // special margins for screen editing?
  env (PAGE_SINGLE)      = "false";     // display page by page
  env (PAGE_PACKET)      = "1";         // display pages by packets of 1,2,...
  env (PAGE_OFFSET)      = "0";         // first page's offset in first packet
  env (PAGE_BORDER)      = "default";   // page border decorations
  env (PAGE_BREAKING)    = "professional";  // quality of page breaking
  env (PAGE_FLEXIBILITY) = "1";         // flexibility factor of stretch
  env (PAGE_FIRST)       = "1";         // number of first page
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
  env (PAGE_THIS_BG_COLOR) = "";        // background color of this page
  env (PAGE_SCREEN_WIDTH)  = "10cm";    // width of current window (for auto)
  env (PAGE_SCREEN_HEIGHT) = "10cm";    // height of current window (for auto)
  env (PAGE_SCREEN_LEFT) = "5mm";       // left margin for screen editing
  env (PAGE_SCREEN_RIGHT)= "5mm";       // right margin for screen editing
  env (PAGE_SCREEN_TOP)  = "15mm";      // top margin for screen editing
  env (PAGE_SCREEN_BOT)  = "15mm";      // bottom margin for screen editing
  env (PAGE_SHOW_HF)     = "true";      // show header and footer
  env (PAGE_FNOTE_SEP)   = "1.0fn";     // space between text & footnotes
  env (PAGE_FNOTE_BARLEN)= "7.5fn";     // length of footnote separating bar
  env (PAGE_FLOAT_SEP)   = "1.5fn";     // space between text & floats
  env (PAGE_FLOAT_ENABLE)= "paper";     // enable floating objects
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
  env (TABLE_BLOCK)      = "no";        // consider table as block content
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
  env (CELL_LSEP)        = "1spc";      // left cell padding
  env (CELL_RSEP)        = "1spc";      // right cell padding
  env (CELL_BSEP)        = "1sep";      // bottom cell padding
  env (CELL_TSEP)        = "1sep";      // top cell padding
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
  env (CELL_SWELL)       = "0ex";       // extra padding around large cells

  env (GR_GEOMETRY)         = gr_geometry;    // geometry of graphics
  env (GR_FRAME)            = gr_frame;       // coordinate frame for graphics
  env (GR_MODE)             = "line";         // graphical mode
  env (GR_AUTO_CROP)        = "false";        // auto crop graphics
  env (GR_CROP_PADDING)     = "1spc";         // padding when auto cropping
  env (GR_GRID)             = gr_grid;        // grid for graphics
  env (GR_GRID_ASPECT)      = gr_grid_aspect; // grid aspect
  env (GR_EDIT_GRID)        = gr_edit_grid;   // edit grid
  env (GR_EDIT_GRID_ASPECT) = gr_grid_aspect; // edit grid (subdivisions)
  env (GR_TRANSFORMATION)   = gr_transf;      // 3D transformation
  env (GR_SNAP_DISTANCE)    = "10px";         // default snap distance

  env (GR_GID)           = "default";   // graphical identifier of new objects
  env (GR_ANIM_ID)       = "default";   // animation identifier of new objects
  env (GR_PROVISO)       = "default";   // visibility condition for new objects
  env (GR_MAGNIFY)       = "default";   // magnify of new objects
  env (GR_OPACITY)       = "default";   // opacity of new objects
  env (GR_COLOR)         = "default";   // color of new objects
  env (GR_POINT_STYLE)   = "default";   // point style of new objects
  env (GR_POINT_SIZE)    = "default";   // point size of new objects
  env (GR_POINT_BORDER)  = "default";   // point border width for new objects
  env (GR_LINE_WIDTH)    = "default";   // line width for new objects
  env (GR_LINE_JOIN)     = "default";   // line join for new objects
  env (GR_LINE_CAPS)     = "default";   // line caps for new objects
  env (GR_LINE_EFFECTS)    = "default";   // line effects for new objects
  env (GR_LINE_PORTION)    = "default";   // curve portion for new objects
  env (GR_DASH_STYLE)      = "default";   // dash style for new objects
  env (GR_DASH_STYLE_UNIT) = "default";   // dash style unit for new objects
  env (GR_ARROW_BEGIN)     = "default";   // arrow begin for new objects
  env (GR_ARROW_END)       = "default";   // arrow end for new objects
  env (GR_ARROW_LENGTH)    = "default";   // arrow length for new objects
  env (GR_ARROW_HEIGHT)    = "default";   // arrow height for new objects
  env (GR_FILL_COLOR)      = "default";   // fill color for new objects
  env (GR_FILL_STYLE)      = "default";   // fill style for new objects
  env (GR_TEXT_AT_HALIGN)  = "default";   // horiz. alignment for new text-ats
  env (GR_TEXT_AT_VALIGN)  = "default";   // vert. alignment for new text-ats
  env (GR_TEXT_AT_MARGIN)  = "default";   // margins for new text-ats
  env (GR_DOC_AT_VALIGN)   = "default";   // vert. al. for new document-ats
  env (GR_DOC_AT_WIDTH)    = "default";   // width of new document-ats
  env (GR_DOC_AT_HMODE)    = "default";   // width mode for new document-ats
  env (GR_DOC_AT_PPSEP)    = "default";   // par-par-sep for new document-ats
  env (GR_DOC_AT_BORDER)   = "default";   // border of new document-ats
  env (GR_DOC_AT_PADDING)  = "default";   // padding for new document-ats

  env (GID)              = "default";   // graphical identifier
  env (ANIM_ID)          = "";          // identifier inside animations
  env (PROVISO)          = "true";      // visibility condition
  env (MAGNIFY)          = "1";         // magnification for graphical objects
  env (POINT_STYLE)      = "disk";      // point style (square, circle...)
  env (POINT_SIZE)       = "2.5ln";     // point size
  env (POINT_BORDER)     = "1ln";       // point border width
  env (LINE_WIDTH)       = "1ln";       // line width in graphics
  env (LINE_JOIN)        = "normal";    // junctions in multilines
  env (LINE_CAPS)        = "normal";    // caps at ends
  env (LINE_EFFECTS)     = "none";      // effects to be applied on line
  env (LINE_PORTION)     = "1";         // portion of curve to be drawn
  env (DASH_STYLE)       = "none";      // dash style
  env (DASH_STYLE_UNIT)  = "5ln";       // dash style unit
  env (ARROW_BEGIN)      = "none";      // arrow at beginning of line
  env (ARROW_END)        = "none";      // arrow at end of line
  env (ARROW_LENGTH)     = "5ln";       // longitudal length of arrow
  env (ARROW_HEIGHT)     = "5ln";       // transverse height of arrow
  env (FILL_COLOR)       = "none";      // fill color
  env (FILL_STYLE)       = "plain";     // fill style
  env (TEXT_AT_HALIGN)   = "left";      // horizontal text-at alignment
  env (TEXT_AT_VALIGN)   = "base";      // vertical text-at alignment
  env (TEXT_AT_MARGIN)   = "1spc";      // margin for smart guides
  env (DOC_AT_VALIGN)    = "top";       // vertical document-at alignment
  env (DOC_AT_WIDTH)     = "1par";      // width of document-ats
  env (DOC_AT_HMODE)     = "min";       // width mode for document-ats
  env (DOC_AT_PPSEP)     = "0fn";       // par-par-sep for document-ats
  env (DOC_AT_BORDER)    = "0ln";       // border of document-ats
  env (DOC_AT_PADDING)   = "0spc";      // padding for document-ats

  env (SRC_STYLE)        = "angular";   // style for "source" tags
  env (SRC_SPECIAL)      = "normal";    // special treatment of certain tags
  env (SRC_COMPACT)      = "normal";    // compact inline/multi-paragraph tags?
  env (SRC_CLOSE)        = "compact";   // how to close long tags
  env (SRC_TAG_COLOR)    = "blue";      // default source tag color

  env (CANVAS_TYPE)           = "plain";        // which kind of scrollbars
  env (CANVAS_COLOR)          = "white";        // canvas colour
  env (CANVAS_HPADDING)       = "0px";          // horizontal canvas padding
  env (CANVAS_VPADDING)       = "0px";          // vertical canvas padding
  env (CANVAS_BAR_WIDTH)      = "1em";          // width of scroll bars
  env (CANVAS_BAR_PADDING)    = "0.25em";       // distance of scrollbars
  env (CANVAS_BAR_COLOR)      = "grey";         // color of bar
  env (ORNAMENT_SHAPE)        = "classic";      // shape of the ornament
  env (ORNAMENT_TITLE_STYLE)  = "classic";      // title style
  env (ORNAMENT_BORDER)       = "1ln";          // border width
  env (ORNAMENT_SWELL)        = "0.5";          // border swell
  env (ORNAMENT_CORNER)       = "100%";         // corner rounding
  env (ORNAMENT_HPADDING)     = "1spc";         // horizontal padding of body
  env (ORNAMENT_VPADDING)     = "1spc";         // vertical padding of body
  env (ORNAMENT_COLOR)        = "";             // background color
  env (ORNAMENT_EXTRA_COLOR)  = "white";        // background color for titles
  env (ORNAMENT_SUNNY_COLOR)  = "black";        // sunny color
  env (ORNAMENT_SHADOW_COLOR) = "black";        // shadow color

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
  tree act_id (ID, tree (HARD_ID, tree (ARG, "args", "0")));
  tree act_script (MAP_ARGS, "find-accessible", "script", "args", "1");
  tree act_ln (LINK, "action", copy (act_id), copy (act_script));
  env ("hlink")= tree (MACRO, "body", "destination",
		       tree (LOCUS, copy (src_id), ln1,
                             tree (ARG, "body")));
  env ("action")= tree (XMACRO, "args",
			tree (LOCUS, copy (act_id), copy (act_ln),
                              tree (ARG, "args", "0")));
  env ("label")= tree (MACRO, "Id", 
		       tree (LOCUS, copy (ref_id), ln4,
			     tree (CONCAT, labflag, labtxt)));
  env ("tag")= tree (MACRO, "Id", "body",
		     tree (WITH, "the-tags", merged,
			   tree (SURROUND, tagflag, "",
                                 tree (ARG, "body"))));
  env ("reference")= tree (MACRO, "Id",
			   tree (LOCUS, copy (ref_id), ln3, reftxt));
  env ("pageref")= tree (MACRO, "Id",
			 tree (LOCUS, copy (ref_id), copy (ln3), preftxt));
  env ("include")= tree (MACRO, "name",
			 tree (VAR_INCLUDE, tree (ARG, "name")));

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
  env ("phantom-float")=
    tree (MACRO, "type", "pos",
          tree (CONCAT, tree (FLAG, "float", "brown"),
                tree (_FLOAT,  tree (ARG, "type"), tree (ARG, "pos"),
                      tree (DOCUMENT))));

  /* syntactic highlighting */
  env ("src-regular")=
    tree (MACRO, "body", tree (WITH, COLOR, "black", tree (ARG, "body")));
  env ("src-macro")=
    tree (MACRO, "body", tree (WITH, MODE, "src",
                               COLOR, tree (VALUE, SRC_TAG_COLOR),
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
