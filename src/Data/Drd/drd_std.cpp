
/******************************************************************************
* MODULE     : drd_std.cpp
* DESCRIPTION: standard drd for TeXmacs; most other drd's inherit from it
* COPYRIGHT  : (C) 2003  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "drd_std.hpp"

drd_info std_drd ("tm");
drd_info the_drd= std_drd;
hashmap<string,int> STD_CODE (UNKNOWN);

#define BIFORM   CHILD_BIFORM
#define DETAILED CHILD_DETAILED

static tag_info
fixed (int arity, int extra=0, int child_mode= CHILD_UNIFORM) {
  return tag_info (arity, extra, ARITY_NORMAL, child_mode, true);
}

static tag_info
options (int arity, int extra, int child_mode= CHILD_UNIFORM) {
  return tag_info (arity, extra, ARITY_OPTIONS, child_mode, true);
}

static tag_info
repeat (int arity, int extra, int child_mode= CHILD_UNIFORM) {
  return tag_info (arity, extra, ARITY_REPEAT, child_mode, true);
}

static tag_info
var_repeat (int arity, int extra, int child_mode= CHILD_UNIFORM) {
  return tag_info (extra, arity, ARITY_VAR_REPEAT, child_mode, true);
}

static void
init (tree_label l, string name, tag_info ti) {
  STD_CODE(name)= (int) l;
  make_tree_label (l, name);
  std_drd->info (l)= ti;
  std_drd->freeze_arity (l);
  std_drd->freeze_no_border (l);
  // std_drd->freeze_block (l);
  // FIXME: freeze children properties
}

static bool std_drd_initialized= false;

void
init_std_drd () {
  if (std_drd_initialized) return;
  std_drd_initialized=true;

  init (STRING, "string", fixed (0));
  init (UNKNOWN, "unknown", fixed (0));
  init (UNINIT, "uninit", fixed (0));
  init (ERROR, "error", fixed (1));
  init (RAW_DATA, "raw-data", fixed (1));

  init (DOCUMENT, "document", repeat (1, 1) -> no_border () -> accessible (0));
  init (PARA, "para",
	repeat (1, 1) -> no_border () -> accessible (0) -> name ("paragraph"));
  init (SURROUND, "surround", fixed (3) -> accessible (0));
  init (CONCAT, "concat", repeat (1, 1) -> no_border () -> accessible (0));
  init (GROUP, "group", fixed (1) -> accessible (0));
  init (HIDDEN, "hidden", fixed (1) -> no_border () -> hidden (0));
  init (FREEZE, "freeze", fixed (1) -> no_border ());
  init (UNFREEZE, "unfreeze", fixed (1) -> accessible (0) -> no_border ());
  init (HSPACE, "hspace", options (1, 2) -> name ("horizontal space"));
  init (VAR_VSPACE, "vspace*",
	options (1, 2) -> name ("vertical space before"));
  init (VSPACE, "vspace", options (1, 2) -> name ("vertical space"));
  init (SPACE, "space", options (1, 2)); // arity 1 or 3
  // space markup has arity 1 or 3
  init (HTAB, "htab", options (1, 1) -> name ("tab"));
  init (MOVE, "move", fixed (1, 2, BIFORM) -> accessible (0));
  init (RESIZE, "resize", fixed (1, 4, BIFORM) -> accessible (0));
  init (CLIPPED, "clipped", fixed (4, 1, BIFORM) -> accessible (1));
  init (REPEAT, "repeat", fixed (1, 1, BIFORM) -> accessible (0));
  init (_FLOAT, "float", fixed (2, 1, BIFORM) -> accessible (1));
  init (DATOMS, "datoms",
	var_repeat (1, 1, BIFORM) -> accessible (1) ->
	name ("decorate atoms"));
  // arbitrary number of macros and decorated content
  init (DLINES, "dlines",
	var_repeat (1, 1, BIFORM) -> accessible (1) ->
	name ("decorate lines"));
  init (DPAGES, "dpages",
	var_repeat (1, 1, BIFORM) -> accessible (1) ->
	name ("decorate pages"));
  init (DBOX, "dbox", fixed (0) -> name ("decorated box"));

  init (WITH_LIMITS, "with-limits", fixed (0) -> name ("with limits"));
  init (LINE_BREAK, "line-break", fixed (0) -> name ("line break"));
  init (NEW_LINE, "new-line", fixed (0) -> name ("new line"));
  init (NEXT_LINE, "next-line", fixed (0) -> name ("next line"));
  init (NO_BREAK, "no-break", fixed (0) -> name ("no line break"));
  init (NO_INDENT, "no-indent", fixed (0) -> name ("don't indent"));
  init (YES_INDENT, "yes-indent", fixed (0) -> name ("do indent"));
  init (VAR_NO_INDENT, "no-indent*", fixed (0) -> name ("don't indent after"));
  init (VAR_YES_INDENT, "yes-indent*", fixed (0) -> name ("do indent after"));
  init (VAR_PAGE_BREAK, "page-break*",
	fixed (0) -> name ("page break before"));
  init (PAGE_BREAK, "page-break", fixed (0) -> name ("page break"));
  init (VAR_NO_PAGE_BREAK, "no-page-break*",
	fixed (0) -> name ("no page break before"));
  init (NO_PAGE_BREAK, "no-page-break", fixed (0) -> name ("no page break"));
  init (VAR_NEW_PAGE, "new-page*", fixed (0) -> name ("new page before"));
  init (NEW_PAGE, "new-page", fixed (0) -> name ("new page"));
  init (VAR_NEW_DPAGE, "new-dpage*",
	fixed (0) -> name ("new double page before"));
  init (NEW_DPAGE, "new-dpage", fixed (0) -> name ("new double page"));

  init (LEFT, "left", options (1, 2));
  init (MID, "mid", options (1, 2));
  init (RIGHT, "right", options (1, 2));
  init (BIG, "big", fixed (1));
  init (LPRIME, "lprime", fixed (1) -> name ("left prime"));
  init (RPRIME, "rprime", fixed (1) -> name ("right prime"));
  init (BELOW, "below", fixed (2) -> accessible (0));
  init (ABOVE, "above", fixed (2) -> accessible (0));
  init (LSUB, "lsub",
	fixed (1) -> accessible (0) -> name ("left subscript"));
  init (LSUP, "lsup",
	fixed (1) -> accessible (0) -> name ("left superscript"));
  init (RSUB, "rsub",
	fixed (1) -> accessible (0) -> name ("subscript"));
  init (RSUP, "rsup",
	fixed (1) -> accessible (0) -> name ("superscript"));
  init (FRAC, "frac", fixed (2) -> accessible (0) -> name ("fraction"));
  init (SQRT, "sqrt", options (1, 1) -> accessible (0) -> name ("root"));
  init (WIDE, "wide", fixed (1, 1, BIFORM) -> accessible (0));
  init (VAR_WIDE, "wide*",
	fixed (1, 1, BIFORM) -> accessible (0) -> name ("wide under"));
  init (NEG, "neg", fixed (1) -> accessible (0) -> name ("negation"));
  init (TREE, "tree", repeat (2, 1) -> accessible (0));

  init (TFORMAT, "tformat",
	var_repeat (1, 1, BIFORM) -> no_border () -> accessible (1) ->
	name ("table format"));
  init (TWITH, "twith",
	fixed (2) -> accessible (0) -> name ("table property"));
  init (CWITH, "cwith",
	fixed (6) -> accessible (0) -> name ("cell property"));
  init (TMARKER, "tmarker", fixed (0) -> name ("table marker"));
  init (TABLE, "table", repeat (1, 1) -> no_border () -> accessible (0));
  init (ROW, "row", repeat (1, 1) -> no_border () -> accessible (0));
  init (CELL, "cell", fixed (1) -> no_border () -> accessible (0));
  init (SUBTABLE, "subtable", fixed (1) -> no_border () -> accessible (0));

  init (ASSIGN, "assign", fixed (2));
  init (WITH, "with", var_repeat (2, 1, BIFORM) -> accessible (1));
  init (PROVIDES, "provides", fixed (1));
  init (VALUE, "value", fixed (1));
  init (QUOTE_VALUE, "quote-value", fixed (1) -> name ("quoted value"));
  init (MACRO, "macro", var_repeat (1, 1) -> accessible (0));
  init (DRD_PROPS, "drd-props", repeat (3, 2) -> name ("drd properties"));
  init (QUOTE_ARG, "quote-arg", repeat (1, 1) -> name ("quoted argument"));
  init (ARG, "arg", repeat (1, 1) -> name ("argument"));
  init (COMPOUND, "compound", repeat (1, 1, BIFORM) -> accessible (1));
  // FIXME: should be refined. The current setting is f.i. needed for "theorem"
  init (XMACRO, "xmacro", fixed (2) -> accessible (0));
  init (GET_LABEL, "get-label", fixed (1));
  init (GET_ARITY, "get-arity", fixed (1));
  init (MAP_ARGS, "map-args", options (3, 2) -> name ("map arguments"));
  init (EVAL_ARGS, "eval-args", fixed (1) -> name ("evaluate arguments"));
  init (MARK, "mark", fixed (2));
  init (EXPAND_AS, "expand-as", fixed (2));
  init (EVAL, "eval", fixed (1) -> name ("evaluate"));
  init (QUOTE, "quote", fixed (1));
  init (QUASI, "quasi", fixed (1));
  init (QUASIQUOTE, "quasiquote", fixed (1));
  init (UNQUOTE, "unquote", fixed (1));
  init (VAR_UNQUOTE, "unquote*", fixed (1));
  init (COPY, "copy", fixed (1));
  init (IF, "if", options (2, 1));
  init (VAR_IF, "if*", fixed (2));
  init (CASE, "case", repeat (2, 1));
  init (WHILE, "while", fixed (2));
  init (FOR_EACH, "for-each", fixed (2));
  init (EXTERN, "extern", repeat (1, 1)); // func and args
  init (INCLUDE, "include", fixed (1));
  init (USE_PACKAGE, "use-package", repeat (1, 1));
  init (USE_MODULE, "use-module", repeat (1, 1));

  init (OR, "or", repeat (2, 1));
  init (XOR, "xor", fixed (2));
  init (AND, "and", repeat (2, 1));
  init (NOT, "not", fixed (1));
  init (PLUS, "plus", repeat (2, 1));
  init (MINUS, "minus", repeat (1, 1));
  init (TIMES, "times", repeat (2, 1));
  init (OVER, "over", repeat (1, 1));
  init (DIV, "div", fixed (2) -> name ("divide"));
  init (MOD, "mod", fixed (2) -> name ("modulo"));
  init (MATH_SQRT, "math-sqrt", fixed (1));
  init (EXP, "exp", fixed (1));
  init (LOG, "log", fixed (1));
  init (POW, "pow", fixed (2));
  init (COS, "cos", fixed (1));
  init (SIN, "sin", fixed (1));
  init (TAN, "tan", fixed (1));
  init (MERGE, "merge", repeat (2, 1));
  init (LENGTH, "length", fixed (1));
  init (RANGE, "range", fixed (1, 2, BIFORM) -> accessible (0));
  init (NUMBER, "number", fixed (2));
  init (_DATE, "date", options (0, 2));
  init (TRANSLATE, "translate", fixed (3));
  init (CHANGE_CASE, "change-case", fixed (1, 1, BIFORM) -> accessible (0));
  init (FIND_FILE, "find-file", var_repeat (1, 1)); // dirs and file
  init (IS_TUPLE, "is-tuple", fixed (1) -> name ("tuple?"));
  init (LOOK_UP, "look-up", fixed (2));
  init (EQUAL, "equal", fixed (2));
  init (UNEQUAL, "unequal", fixed (2) -> name ("not equal"));
  init (LESS, "less", fixed (2));
  init (LESSEQ, "lesseq", fixed (2) -> name ("less or equal"));
  init (GREATER, "greater", fixed (2));
  init (GREATEREQ, "greatereq", fixed (2) -> name ("greater or equal"));

  init (CM_LENGTH, "cm-length", fixed (0));
  init (MM_LENGTH, "mm-length", fixed (0));
  init (IN_LENGTH, "in-length", fixed (0));
  init (PT_LENGTH, "pt-length", fixed (0));
  init (BP_LENGTH, "bp-length", fixed (0));
  init (DD_LENGTH, "dd-length", fixed (0));
  init (PC_LENGTH, "pc-length", fixed (0));
  init (CC_LENGTH, "cc-length", fixed (0));
  init (FS_LENGTH, "fs-length", fixed (0));
  init (FBS_LENGTH, "fbs-length", fixed (0));
  init (EM_LENGTH, "em-length", fixed (0));
  init (LN_LENGTH, "ln-length", fixed (0));
  init (SEP_LENGTH, "sep-length", fixed (0));
  init (YFRAC_LENGTH, "yfrac-length", fixed (0));
  init (EX_LENGTH, "ex-length", fixed (0));
  init (FN_LENGTH, "fn-length", fixed (0));
  init (FNS_LENGTH, "fns-length", fixed (0));
  init (BLS_LENGTH, "bls-length", fixed (0));
  init (FNBOT_LENGTH, "fnbot-length", fixed (0));
  init (FNTOP_LENGTH, "fntop-length", fixed (0));
  init (SPC_LENGTH, "spc-length", fixed (0));
  init (XSPC_LENGTH, "xspc-length", fixed (0));
  init (PAR_LENGTH, "par-length", fixed (0));
  init (PAG_LENGTH, "pag-length", fixed (0));
  init (GW_LENGTH, "gw-length", fixed (0));
  init (GH_LENGTH, "gh-length", fixed (0));
  init (TMPT_LENGTH, "tmpt-length", fixed (0));
  init (PX_LENGTH, "px-length", fixed (0));
  init (MSEC_LENGTH, "msec-length", fixed (0));
  init (SEC_LENGTH, "sec-length", fixed (0));
  init (MIN_LENGTH, "min-length", fixed (0));
  init (H_LENGTH, "h-length", fixed (0));

  init (STYLE_WITH, "style-with",
	var_repeat (2, 1, BIFORM) -> accessible (1));
  init (VAR_STYLE_WITH, "style-with*",
	var_repeat (2, 1, BIFORM) -> accessible (1));
  init (STYLE_ONLY, "style-only", fixed (1) -> accessible (0));
  init (VAR_STYLE_ONLY, "style-only*", fixed (1) -> accessible (0));
  init (ACTIVE, "active", fixed (1) -> accessible (0));
  init (VAR_ACTIVE, "active*", fixed (1) -> accessible (0));
  init (INACTIVE, "inactive", fixed (1) -> accessible (0));
  init (VAR_INACTIVE, "inactive*", fixed (1) -> accessible (0));
  init (REWRITE_INACTIVE, "rewrite-inactive", fixed (2));
  init (INLINE_TAG, "inline-tag", repeat (1, 1) -> accessible (0));
  init (OPEN_TAG, "open-tag", repeat (1, 1) -> accessible (0));
  init (MIDDLE_TAG, "middle-tag", repeat (1, 1, BIFORM) -> accessible (1));
  init (CLOSE_TAG, "close-tag", repeat (1, 1, BIFORM) -> accessible (1));
  init (SYMBOL, "symbol", fixed (1));
  init (LATEX, "latex", fixed (1));
  init (HYBRID, "hybrid", options (1, 1));
  init (HIGHLIGHT, "highlight", fixed (1) -> accessible (0));

  init (LOCUS, "locus", var_repeat (1, 1, BIFORM) -> accessible (1));
  init (ID, "id", repeat (1, 1) -> accessible (0));
  init (HARD_ID, "hard-id", options (0, 1));
  init (LINK, "link", repeat (2, 1) -> accessible (0));
  init (URL, "url", options (1, 1) -> accessible (0));
  init (SCRIPT, "script", options (1, 1) -> accessible (0));
  init (HLINK, "hlink",
	fixed (1, 1, BIFORM) -> accessible (0) -> name ("hyperlink"));
  init (ACTION, "action", options (2, 1, DETAILED) -> accessible (0));
  init (SET_BINDING, "set-binding", options (1, 2));
  init (GET_BINDING, "get-binding", options (1, 1));
  init (LABEL, "label", fixed (1));
  init (REFERENCE, "reference", fixed (1));
  init (PAGEREF, "pageref", fixed (1) -> name ("page reference"));
  init (WRITE, "write", fixed (2));

  init (TUPLE, "tuple", repeat (0, 1) -> accessible (0));
  init (ATTR, "attr", repeat (2, 2) -> accessible (0) -> name ("attributes"));
  init (TMLEN, "tmlen", options (1, 2) -> name ("TeXmacs length"));
  init (COLLECTION, "collection", repeat (1, 1));
  init (ASSOCIATE, "associate", fixed (2));
  init (BACKUP, "backup", fixed (2));
  init (PATTERN, "pattern", options (3, 1));
  init (GRADIENT, "gradient", fixed (3));
  init (SPECIFIC, "specific", fixed (2));
  init (FLAG, "flag", options (2, 1));

  init (ANIM_COMPOSE, "anim-compose", repeat (1, 1));
  init (ANIM_REPEAT, "anim-repeat", fixed (1) -> accessible (0));
  init (ANIM_CONSTANT, "anim-constant",
	fixed (1, 1, BIFORM) -> accessible (0));
  init (ANIM_TRANSLATE, "anim-translate",
	fixed (1, 3, BIFORM) -> accessible (0));
  init (ANIM_PROGRESSIVE, "anim-progressive",
	fixed (1, 3, BIFORM) -> accessible (0));
  init (VIDEO, "video", fixed (5));
  init (SOUND, "sound", fixed (1));

  init (GRAPHICS, "graphics", repeat (1, 1) -> accessible (0));
  init (SUPERPOSE, "superpose", repeat (1, 1) -> accessible (0));
  init (GR_GROUP, "gr-group", repeat (1, 1));
  init (GR_LINEAR_TRANSFORM, "gr-linear-transform", fixed (2));
  init (TEXT_AT, "text-at", fixed (1, 1, BIFORM) -> accessible (0));
  init (_POINT, "point", repeat (1, 1));
  init (LINE, "line", repeat (2, 1));
  init (CLINE, "cline", repeat (3, 1));
  init (ARC, "arc", repeat (3, 1));
  init (CARC, "carc", repeat (3, 1));
  init (SPLINE, "spline", repeat (2, 1));
  init (VAR_SPLINE, "spline*", repeat (2, 1));
  init (CSPLINE, "cspline", repeat (2, 1));
  init (FILL, "fill", repeat (1, 1));
  init (POSTSCRIPT, "postscript", fixed (7));
  init (BOX_INFO, "box-info", fixed (2));
  init (FRAME_DIRECT, "frame-direct", fixed (1));
  init (FRAME_INVERSE, "frame-inverse", fixed (1));

  init (CANVAS, "canvas", fixed (6, 1, BIFORM) -> accessible (1));
  init (ORNAMENT, "ornament", fixed (1) -> accessible (0));

  init (FORMAT, "format", repeat (1, 1));
  init (LINE_SEP, "line-sep", fixed (0) -> name ("line separator"));
  init (SPLIT, "split", repeat (1, 1));
  init (DELAY, "delay", fixed (1));
  init (HOLD, "hold", fixed (1));
  init (RELEASE, "release", fixed (1));
  init (OLD_MATRIX, "old-matrix", var_repeat (1, 2, BIFORM) -> accessible (0));
  init (OLD_TABLE, "old-table", var_repeat (1, 2, BIFORM) -> accessible (0));
  init (OLD_MOSAIC, "old-mosaic", var_repeat (1, 2, BIFORM) -> accessible (0));
  init (OLD_MOSAIC_ITEM, "old-mosaic-item", repeat (1, 1) -> accessible (0));
  init (SET, "set", fixed (2));
  init (RESET, "reset", fixed (1));
  init (EXPAND, "expand", repeat (1, 1, BIFORM) -> accessible (1));
  init (VAR_EXPAND, "expand*",
	repeat (1, 1, BIFORM) -> no_border () -> accessible (1));
  init (HIDE_EXPAND, "hide-expand", repeat (2, 1, DETAILED) -> accessible (1));
  init (APPLY, "apply", repeat (1, 1));
  init (BEGIN, "begin", repeat (1, 1));
  init (END, "end", fixed (1));
  init (FUNC, "func", var_repeat (1, 1));
  init (ENV, "env", var_repeat (1, 2));
  init (AUTHORIZE, "authorize", fixed (2));

  init (make_tree_label ("shown"), "shown",
	fixed (1) -> accessible (0) -> no_border ());
  init (make_tree_label ("ignore"), "ignore", fixed (1) -> no_border ());
}
