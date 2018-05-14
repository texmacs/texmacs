
/******************************************************************************
* MODULE     : vars.cpp
* DESCRIPTION: the environment variables
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "vars.hpp"

/******************************************************************************
* Various important environment variables
******************************************************************************/

string DPI ("dpi");
string ZOOM_FACTOR ("zoom-factor");
string PREAMBLE ("preamble");
string SAVE_AUX ("save-aux");
string MODE ("mode");
string INFO_FLAG ("info-flag");
string WINDOW_BARS ("window-bars");
string SCROLL_BARS ("scroll-bars");
string IDENTITY ("identity");
string TABULAR ("tabular");
string THE_LABEL ("the-label");
string THE_TAGS ("the-tags");
string THE_MODULES ("the-modules");
string WARN_MISSING ("warn-missing");
string GLOBAL_TITLE ("global-title");
string GLOBAL_AUTHOR ("global-author");
string GLOBAL_SUBJECT ("global-subject");

/******************************************************************************
* Text properties for tex, math and prog modes
******************************************************************************/

string FONT ("font");
string FONT_FAMILY ("font-family");
string FONT_SERIES ("font-series");
string FONT_SHAPE ("font-shape");
string FONT_SIZE ("font-size");
string FONT_BASE_SIZE ("font-base-size");
string FONT_EFFECTS ("font-effects");
string MAGNIFICATION ("magnification");
string COLOR ("color");
string OPACITY ("opacity");
string BG_COLOR ("bg-color");
string LOCUS_COLOR ("locus-color");
string VISITED_COLOR ("visited-color");
string NO_PATTERNS ("no-patterns");
string LANGUAGE ("language");
string ATOM_DECORATIONS ("atom-decorations");
string LINE_DECORATIONS ("line-decorations");
string PAGE_DECORATIONS ("page-decorations");
string XOFF_DECORATIONS ("xoff-decorations");
string YOFF_DECORATIONS ("yoff-decorations");

string MATH_LANGUAGE ("math-language");
string MATH_FONT ("math-font");
string MATH_FONT_FAMILY ("math-font-family");
string MATH_FONT_SERIES ("math-font-series");
string MATH_FONT_SHAPE ("math-font-shape");
string MATH_LEVEL ("math-level");
string MATH_DISPLAY ("math-display");
string MATH_CONDENSED ("math-condensed");
string MATH_VPOS ("math-vpos");
string MATH_NESTING_MODE ("math-nesting-mode");
string MATH_NESTING_LEVEL ("math-nesting-level");
string MATH_FRAC_LIMIT ("math-frac-limit");
string MATH_TABLE_LIMIT ("math-table-limit");
string MATH_FLATTEN_COLOR ("math-flatten-color");
string MATH_TOP_SWELL_START ("math-top-swell-start");
string MATH_TOP_SWELL_END ("math-top-swell-end");
string MATH_BOT_SWELL_START ("math-bot-swell-start");
string MATH_BOT_SWELL_END ("math-bot-swell-end");

string PROG_LANGUAGE ("prog-language");
string PROG_SCRIPTS ("prog-scripts");
string PROG_FONT ("prog-font");
string PROG_FONT_FAMILY ("prog-font-family");
string PROG_FONT_SERIES ("prog-font-series");
string PROG_FONT_SHAPE ("prog-font-shape");
string PROG_SESSION ("prog-session");

/******************************************************************************
* Environment variables for paragraphs
******************************************************************************/

string PAR_MODE ("par-mode");
string PAR_FLEXIBILITY ("par-flexibility");
string PAR_HYPHEN ("par-hyphen");
string PAR_SPACING ("par-spacing");
string PAR_KERNING_REDUCE ("par-kerning-reduce");
string PAR_KERNING_STRETCH ("par-kerning-stretch");
string PAR_KERNING_MARGIN ("par-kerning-margin");
string PAR_CONTRACTION ("par-contraction");
string PAR_EXPANSION ("par-expansion");
string PAR_WIDTH ("par-width");
string PAR_LEFT ("par-left");
string PAR_RIGHT ("par-right");
string PAR_FIRST ("par-first");
string PAR_NO_FIRST ("par-no-first");
string PAR_SEP ("par-sep");
string PAR_HOR_SEP ("par-hor-sep");
string PAR_VER_SEP ("par-ver-sep");
string PAR_LINE_SEP ("par-line-sep");
string PAR_PAR_SEP ("par-par-sep");
string PAR_FNOTE_SEP ("par-fnote-sep");
string PAR_COLUMNS ("par-columns");
string PAR_COLUMNS_SEP ("par-columns-sep");
string PAR_SWELL ("par-swell");

/******************************************************************************
* Environment variables for pages
******************************************************************************/

string PAGE_MEDIUM ("page-medium");
string PAGE_PRINTED ("page-printed");
string PAGE_TYPE ("page-type");
string PAGE_ORIENTATION ("page-orientation");
string PAGE_WIDTH_MARGIN ("page-width-margin");
string PAGE_HEIGHT_MARGIN ("page-height-margin");
string PAGE_SCREEN_MARGIN ("page-screen-margin");
string PAGE_SINGLE ("page-single");
string PAGE_PACKET ("page-packet");
string PAGE_OFFSET ("page-offset");
string PAGE_BORDER ("page-border");
string PAGE_BREAKING ("page-breaking");
string PAGE_FLEXIBILITY ("page-flexibility");
string PAGE_FIRST ("page-first");
string PAGE_NR ("page-nr");
string PAGE_THE_PAGE ("page-the-page");
string PAGE_WIDTH ("page-width");
string PAGE_HEIGHT ("page-height");
string PAGE_ODD ("page-odd");
string PAGE_EVEN ("page-even");
string PAGE_RIGHT ("page-right");
string PAGE_ODD_SHIFT ("page-odd-shift");
string PAGE_EVEN_SHIFT ("page-even-shift");
string PAGE_TOP ("page-top");
string PAGE_BOT ("page-bot");
string PAGE_USER_HEIGHT ("page-user-height");
string PAGE_EXTEND ("page-extend");
string PAGE_SHRINK ("page-shrink");
string PAGE_HEAD_SEP ("page-head-sep");
string PAGE_FOOT_SEP ("page-foot-sep");
string PAGE_ODD_HEADER ("page-odd-header");
string PAGE_ODD_FOOTER ("page-odd-footer");
string PAGE_EVEN_HEADER ("page-even-header");
string PAGE_EVEN_FOOTER ("page-even-footer");
string PAGE_THIS_TOP ("page-this-top");
string PAGE_THIS_BOT ("page-this-bot");
string PAGE_THIS_HEADER ("page-this-header");
string PAGE_THIS_FOOTER ("page-this-footer");
string PAGE_THIS_BG_COLOR ("page-this-bg-color");
string PAGE_SCREEN_WIDTH ("page-screen-width");
string PAGE_SCREEN_HEIGHT ("page-screen-height");
string PAGE_SCREEN_LEFT ("page-screen-left");
string PAGE_SCREEN_RIGHT ("page-screen-right");
string PAGE_SCREEN_TOP ("page-screen-top");
string PAGE_SCREEN_BOT ("page-screen-bot");
string PAGE_SHOW_HF ("page-show-hf");
string PAGE_FNOTE_SEP ("page-fnote-sep");
string PAGE_FNOTE_BARLEN ("page-fnote-barlen");
string PAGE_FLOAT_SEP ("page-float-sep");
string PAGE_FLOAT_ENABLE ("page-float-enable");
string PAGE_MNOTE_SEP ("page-mnote-sep");
string PAGE_MNOTE_WIDTH ("page-mnote-width");

/******************************************************************************
* Environment variables for tables
******************************************************************************/

string TABLE_WIDTH ("table-width");
string TABLE_HEIGHT ("table-height");
string TABLE_HMODE ("table-hmode");
string TABLE_VMODE ("table-vmode");
string TABLE_HALIGN ("table-halign");
string TABLE_VALIGN ("table-valign");
string TABLE_ROW_ORIGIN ("table-row-origin");
string TABLE_COL_ORIGIN ("table-col-origin");
string TABLE_LSEP ("table-lsep");
string TABLE_RSEP ("table-rsep");
string TABLE_BSEP ("table-bsep");
string TABLE_TSEP ("table-tsep");
string TABLE_LBORDER ("table-lborder");
string TABLE_RBORDER ("table-rborder");
string TABLE_BBORDER ("table-bborder");
string TABLE_TBORDER ("table-tborder");
string TABLE_HYPHEN ("table-hyphen");
string TABLE_BLOCK ("table-block");
string TABLE_MIN_ROWS ("table-min-rows");
string TABLE_MIN_COLS ("table-min-cols");
string TABLE_MAX_ROWS ("table-max-rows");
string TABLE_MAX_COLS ("table-max-cols");

/******************************************************************************
* Environment variables for cells of tables
******************************************************************************/

string CELL_FORMAT ("cell-format");
string CELL_DECORATION ("cell-decoration");
string CELL_BACKGROUND ("cell-background");
string CELL_ORIENTATION ("cell-orientation");
string CELL_WIDTH ("cell-width");
string CELL_HEIGHT ("cell-height");
string CELL_HPART ("cell-hpart");
string CELL_VPART ("cell-vpart");
string CELL_HMODE ("cell-hmode");
string CELL_VMODE ("cell-vmode");
string CELL_HALIGN ("cell-halign");
string CELL_VALIGN ("cell-valign");
string CELL_LSEP ("cell-lsep");
string CELL_RSEP ("cell-rsep");
string CELL_BSEP ("cell-bsep");
string CELL_TSEP ("cell-tsep");
string CELL_LBORDER ("cell-lborder");
string CELL_RBORDER ("cell-rborder");
string CELL_BBORDER ("cell-bborder");
string CELL_TBORDER ("cell-tborder");
string CELL_VCORRECT ("cell-vcorrect");
string CELL_HYPHEN ("cell-hyphen");
string CELL_BLOCK ("cell-block");
string CELL_ROW_SPAN ("cell-row-span");
string CELL_COL_SPAN ("cell-col-span");
string CELL_ROW_NR ("cell-row-nr");
string CELL_COL_NR ("cell-col-nr");
string CELL_SWELL ("cell-swell");

/******************************************************************************
* Environment variables for graphics
******************************************************************************/

string GR_GEOMETRY ("gr-geometry");
string GR_FRAME ("gr-frame");
string GR_MODE ("gr-mode");
string GR_AUTO_CROP ("gr-auto-crop");
string GR_CROP_PADDING ("gr-crop-padding");
string GR_GRID ("gr-grid");
string GR_GRID_ASPECT ("gr-grid-aspect");
string GR_EDIT_GRID ("gr-edit-grid");
string GR_EDIT_GRID_ASPECT ("gr-edit-grid-aspect");
string GR_TRANSFORMATION ("gr-transformation");
string GR_SNAP_DISTANCE ("gr-snap-distance");

string GR_GID ("gr-gid");
string GR_ANIM_ID ("gr-anim-id");
string GR_PROVISO ("gr-proviso");
string GR_MAGNIFY ("gr-magnify");
string GR_OPACITY ("gr-opacity");
string GR_COLOR ("gr-color");
string GR_POINT_STYLE ("gr-point-style");
string GR_POINT_SIZE ("gr-point-size");
string GR_POINT_BORDER ("gr-point-border");
string GR_LINE_WIDTH ("gr-line-width");
string GR_LINE_JOIN ("gr-line-join");
string GR_LINE_CAPS ("gr-line-caps");
string GR_LINE_EFFECTS ("gr-line-effects");
string GR_LINE_PORTION ("gr-line-portion");
string GR_DASH_STYLE ("gr-dash-style");
string GR_DASH_STYLE_UNIT ("gr-dash-style-unit");
string GR_ARROW_BEGIN ("gr-arrow-begin");
string GR_ARROW_END ("gr-arrow-end");
string GR_ARROW_LENGTH ("gr-arrow-length");
string GR_ARROW_HEIGHT ("gr-arrow-height");
string GR_FILL_COLOR ("gr-fill-color");
string GR_FILL_STYLE ("gr-fill-style");
string GR_TEXT_AT_HALIGN ("gr-text-at-halign");
string GR_TEXT_AT_VALIGN ("gr-text-at-valign");
string GR_TEXT_AT_MARGIN ("gr-text-at-margin");
string GR_DOC_AT_VALIGN ("gr-doc-at-valign");
string GR_DOC_AT_WIDTH ("gr-doc-at-width");
string GR_DOC_AT_HMODE ("gr-doc-at-hmode");
string GR_DOC_AT_PPSEP ("gr-doc-at-ppsep");
string GR_DOC_AT_BORDER ("gr-doc-at-border");
string GR_DOC_AT_PADDING ("gr-doc-at-padding");

string GID ("gid");
string ANIM_ID ("anim-id");
string PROVISO ("proviso");
string MAGNIFY ("magnify");
string POINT_STYLE ("point-style");
string POINT_SIZE ("point-size");
string POINT_BORDER ("point-border");
string LINE_WIDTH ("line-width");
string LINE_JOIN ("line-join");
string LINE_CAPS ("line-caps");
string LINE_EFFECTS ("line-effects");
string LINE_PORTION ("line-portion");
string DASH_STYLE ("dash-style");
string DASH_STYLE_UNIT ("dash-style-unit");
string ARROW_BEGIN ("arrow-begin");
string ARROW_END ("arrow-end");
string ARROW_LENGTH ("arrow-length");
string ARROW_HEIGHT ("arrow-height");
string FILL_COLOR ("fill-color");
string FILL_STYLE ("fill-style");
string TEXT_AT_HALIGN ("text-at-halign");
string TEXT_AT_VALIGN ("text-at-valign");
string TEXT_AT_MARGIN ("text-at-margin");
string DOC_AT_VALIGN ("doc-at-valign");
string DOC_AT_WIDTH ("doc-at-width");
string DOC_AT_HMODE ("doc-at-hmode");
string DOC_AT_PPSEP ("doc-at-ppsep");
string DOC_AT_BORDER ("doc-at-border");
string DOC_AT_PADDING ("doc-at-padding");

/******************************************************************************
* Environment variables for preamble mode
******************************************************************************/

string SRC_STYLE ("src-style");
string SRC_SPECIAL ("src-special");
string SRC_COMPACT ("src-compact");
string SRC_CLOSE ("src-close");
string SRC_TAG_COLOR ("src-tag-color");

/******************************************************************************
* Environment variables for the graphical user interface
******************************************************************************/

string CANVAS_TYPE ("canvas-type");
string CANVAS_COLOR ("canvas-color");
string CANVAS_HPADDING ("canvas-hpadding");
string CANVAS_VPADDING ("canvas-vpadding");
string CANVAS_BAR_WIDTH ("canvas-bar-width");
string CANVAS_BAR_PADDING ("canvas-bar-padding");
string CANVAS_BAR_COLOR ("canvas-bar-color");
string ORNAMENT_SHAPE ("ornament-shape");
string ORNAMENT_TITLE_STYLE ("ornament-title-style");
string ORNAMENT_BORDER ("ornament-border");
string ORNAMENT_SWELL ("ornament-swell");
string ORNAMENT_HPADDING ("ornament-hpadding");
string ORNAMENT_VPADDING ("ornament-vpadding");
string ORNAMENT_COLOR ("ornament-color");
string ORNAMENT_EXTRA_COLOR ("ornament-extra-color");
string ORNAMENT_SUNNY_COLOR ("ornament-sunny-color");
string ORNAMENT_SHADOW_COLOR ("ornament-shadow-color");
