
/******************************************************************************
* MODULE     : vars.cpp
* DESCRIPTION: the environment variables
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "vars.hpp"

/******************************************************************************
* Important environment variables and actions
******************************************************************************/

string DPI ("dpi");
string SFACTOR ("shrinking factor");
string MAGNIFICATION ("magnification");

string PREAMBLE ("preamble");
string MODE ("mode");
string TEXT_LANGUAGE ("language");
string TEXT_FONT ("font");
string TEXT_FAMILY ("font family");
string TEXT_SHAPE ("font shape");
string TEXT_SERIES ("font series");
string MATH_LANGUAGE ("math language");
string MATH_FONT ("math font");
string MATH_FAMILY ("math font family");
string MATH_SHAPE ("math font shape");
string MATH_SERIES ("math font series");
string PROG_LANGUAGE ("prog language");
string PROG_FONT ("prog font");
string PROG_FAMILY ("prog font family");
string PROG_SHAPE ("prog font shape");
string PROG_SERIES ("prog font series");
string FONT_BASE_SIZE ("font base size");
string FONT_SIZE ("font size");
string INDEX_LEVEL ("index level");
string DISPLAY_STYLE ("formula style");
string MATH_CONDENSED ("math condensed");
string VERTICAL_POS ("vertical position");
string COLOR ("color");
string BACKGROUND_COLOR ("background color");
string THIS_SESSION ("this session");
string INFO_FLAG ("info flag");

string ATOM_DECORATIONS ("atom decorations");
string LINE_DECORATIONS ("line decorations");
string PAGE_DECORATIONS ("page decorations");
string XOFF_DECORATIONS ("xoff decorations");
string YOFF_DECORATIONS ("yoff decorations");

/******************************************************************************
* Environment variables for paragraphs
******************************************************************************/

string PAR_MODE ("paragraph mode");
string PAR_HYPHEN ("paragraph hyphenation");
string PAR_WIDTH ("paragraph width");
string PAR_LEFT ("left margin");
string PAR_RIGHT ("right margin");
string PAR_FIRST ("first indentation");
string PAR_NO_FIRST ("no first indentation");
string PAR_SEP ("interline space");
string PAR_HOR_SEP ("horizontal ink separation");
string PAR_LINE_SEP ("line stretch");
string PAR_PAR_SEP ("interparagraph space");
string PAR_FNOTE_SEP ("interfootnote space");
string PAR_COLUMNS ("nr columns");
string PAR_COLUMNS_SEP ("column separation");

/******************************************************************************
* Environment variables for pages
******************************************************************************/

string PAGE_MEDIUM ("page medium");
string PAGE_TYPE ("page type");
string PAGE_ORIENTATION ("page orientation");
string PAGE_BREAKING ("page breaking");
string PAGE_FLEXIBILITY ("page flexibility");
string PAGE_NR ("page number");
string PAGE_THE_PAGE ("thepage");
string PAGE_WIDTH ("page width");
string PAGE_HEIGHT ("page height");
string PAGE_ODD ("odd page margin");
string PAGE_EVEN ("even page margin");
string PAGE_RIGHT ("page right margin");
string PAGE_TOP ("page top margin");
string PAGE_BOT ("page bottom margin");
string PAGE_EXTEND ("page extend");
string PAGE_SHRINK ("page shrink");
string PAGE_HEAD_SEP ("page header separation");
string PAGE_FOOT_SEP ("page footer separation");
string PAGE_ODD_HEADER ("odd page header");
string PAGE_ODD_FOOTER ("odd page footer");
string PAGE_EVEN_HEADER ("even page header");
string PAGE_EVEN_FOOTER ("even page footer");
string PAGE_THIS_HEADER ("this page header");
string PAGE_THIS_FOOTER ("this page footer");
string PAGE_REDUCE_LEFT ("reduction page left margin");
string PAGE_REDUCE_RIGHT ("reduction page right margin");
string PAGE_REDUCE_TOP ("reduction page top margin");
string PAGE_REDUCE_BOT ("reduction page bottom margin");
string PAGE_SHOW_HF ("show header and footer");
string PAGE_FNOTE_SEP ("footnote separation");
string PAGE_FNOTE_BARLEN ("footnote bar length");
string PAGE_FLOAT_SEP ("float separation");
string PAGE_MNOTE_SEP ("marginal note separation");
string PAGE_MNOTE_WIDTH ("marginal note width");

/******************************************************************************
* Environment variables for tables
******************************************************************************/

string TABLE_WIDTH ("table width");
string TABLE_HEIGHT ("table height");
string TABLE_HMODE ("table hmode");
string TABLE_VMODE ("table vmode");
string TABLE_HALIGN ("table halign");
string TABLE_VALIGN ("table valign");
string TABLE_ROW_ORIGIN ("table row origin");
string TABLE_COL_ORIGIN ("table col origin");
string TABLE_LSEP ("table lsep");
string TABLE_RSEP ("table rsep");
string TABLE_BSEP ("table bsep");
string TABLE_TSEP ("table tsep");
string TABLE_LBORDER ("table lborder");
string TABLE_RBORDER ("table rborder");
string TABLE_BBORDER ("table bborder");
string TABLE_TBORDER ("table tborder");
string TABLE_HYPHEN ("table hyphen");
string TABLE_MIN_ROWS ("table min rows");
string TABLE_MIN_COLS ("table min cols");
string TABLE_MAX_ROWS ("table max rows");
string TABLE_MAX_COLS ("table max cols");

/******************************************************************************
* Environment variables for cells of tables
******************************************************************************/

string CELL_FORMAT ("cell format");
string CELL_DECORATION ("cell decoration");
string CELL_BACKGROUND ("cell background");
string CELL_ORIENTATION ("cell orientation");
string CELL_WIDTH ("cell width");
string CELL_HEIGHT ("cell height");
string CELL_HPART ("cell hpart");
string CELL_VPART ("cell vpart");
string CELL_HMODE ("cell hmode");
string CELL_VMODE ("cell vmode");
string CELL_HALIGN ("cell halign");
string CELL_VALIGN ("cell valign");
string CELL_LSEP ("cell lsep");
string CELL_RSEP ("cell rsep");
string CELL_BSEP ("cell bsep");
string CELL_TSEP ("cell tsep");
string CELL_LBORDER ("cell lborder");
string CELL_RBORDER ("cell rborder");
string CELL_BBORDER ("cell bborder");
string CELL_TBORDER ("cell tborder");
string CELL_VCORRECT ("cell vcorrect");
string CELL_HYPHEN ("cell hyphen");
string CELL_ROW_SPAN ("cell row span");
string CELL_COL_SPAN ("cell col span");
string CELL_ROW_NR ("cell row nr");
string CELL_COL_NR ("cell col nr");

/******************************************************************************
* Other variables
******************************************************************************/

string GR_FRAME ("graphical frame");
string GR_CLIP ("graphical clip");
string GR_LINE_WIDTH ("line width");
string GR_LINE_STYLE ("line style");
string GR_LINE_ARROWS ("line arrows");
string GR_LINE_CAPS ("line caps");
string GR_FILL_MODE ("fill mode");
string GR_FILL_COLOR ("fill color");
string GR_FILL_STYLE ("fill style");
string GR_MODE ("graphical mode");

string IDENTITY ("identity");
string TABULAR ("tabular");
