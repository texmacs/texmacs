
/******************************************************************************
* MODULE     : drd_std.cpp
* DESCRIPTION: standard drd for TeXmacs; most other drd's inherit from it
* COPYRIGHT  : (C) 2003  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "drd_std.hpp"

drd_info std_drd ("tm");
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
}

static bool std_drd_initialized= false;

void
initialize_std_drd () {
  if (std_drd_initialized) return;
  std_drd_initialized=true;

  init (STRING, "string", fixed (0));
  init (UNKNOWN, "unknown", fixed (0));
  init (UNINIT, "uninit", fixed (0));
  init (ERROR, "error", fixed (1));
  init (RAW_DATA, "raw_data", fixed (1));

  init (DOCUMENT, "document", repeat (1, 1) -> no_border () -> accessible (0));
  init (PARA, "para",
	repeat (1, 1) -> no_border () -> accessible (0) -> name ("paragraph"));
  init (SURROUND, "surround", fixed (3) -> accessible (0));
  init (CONCAT, "concat", repeat (1, 1) -> no_border () -> accessible (0));
  init (GROUP, "group", fixed (1) -> accessible (0));
  init (HSPACE, "hspace", options (1, 2) -> name ("horizontal space"));
  init (VAR_VSPACE, "vspace*",
	options (1, 2) -> name ("vertical space before"));
  init (VSPACE, "vspace", options (1, 2) -> name ("vertical space"));
  init (SPACE, "space", options (1, 2)); // arity 1 or 3
  // space markup has arity 1 or 3
  init (HTAB, "htab", options (1, 1) -> name ("tab"));
  init (MOVE, "move", fixed (1, 2, BIFORM) -> accessible (0));
  init (RESIZE, "resize", fixed (1, 4, BIFORM) -> accessible (0));
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

  init (WITH_LIMITS, "with_limits", fixed (0));
  init (LINE_BREAK, "line_break", fixed (0));
  init (NEW_LINE, "new_line", fixed (0));
  init (LINE_SEP, "line_separator", fixed (0));
  init (NEXT_LINE, "next_line", fixed (0));
  init (NO_BREAK, "no_line_break", fixed (0));
  init (NO_FIRST_INDENT, "no_first_indentation", fixed (0));
  init (YES_FIRST_INDENT, "enable_first_indentation", fixed (0));
  init (NO_FIRST_INDENT_AFTER, "no_indentation_after", fixed (0));
  init (YES_FIRST_INDENT_AFTER, "enable_indentation_after", fixed (0));
  init (PAGE_BREAK_BEFORE, "page_break_before", fixed (0));
  init (PAGE_BREAK, "page_break", fixed (0));
  init (NO_PAGE_BREAK_BEFORE, "no_page_break_before", fixed (0));
  init (NO_PAGE_BREAK_AFTER, "no_page_break_after", fixed (0));
  init (NEW_PAGE_BEFORE, "new_page_before", fixed (0));
  init (NEW_PAGE, "new_page", fixed (0));
  init (NEW_DOUBLE_PAGE_BEFORE, "new_double_page_before", fixed (0));
  init (NEW_DOUBLE_PAGE, "new_double_page", fixed (0));

  init (LEFT, "left", fixed (1));
  init (MID, "mid", fixed (1));
  init (RIGHT, "right", fixed (1));
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
  init (SUBTABLE, "sub_table", fixed (1) -> no_border () -> accessible (0));

  init (ASSIGN, "assign", fixed (2));
  init (WITH, "with", var_repeat (2, 1, BIFORM) -> accessible (1));
  init (PROVIDES, "provides", fixed (1));
  init (VALUE, "value", fixed (1));
  init (MACRO, "macro", var_repeat (1, 1) -> accessible (0));
  init (DRD_PROPS, "drd_props", repeat (3, 2) -> name ("drd properties"));
  init (ARG, "arg", repeat (1, 1) -> name ("argument"));
  init (COMPOUND, "compound", repeat (1, 1, BIFORM) -> accessible (1));
  // FIXME: should be refined. The current setting is f.i. needed for "theorem"
  init (XMACRO, "xmacro", fixed (2) -> accessible (0));
  init (GET_LABEL, "get_label", fixed (1));
  init (GET_ARITY, "get_arity", fixed (1));
  init (MAP_ARGS, "map_args", options (3, 2) -> name ("map arguments"));
  init (EVAL_ARGS, "eval_args", fixed (1) -> name ("evaluate arguments"));
  init (EVAL, "eval", fixed (1) -> name ("evaluate"));
  init (QUOTE, "quote", fixed (1));
  init (DELAY, "delay", fixed (1));
  init (HOLD, "hold", fixed (1));
  init (RELEASE, "release", fixed (1));
  init (EXTERN, "extern", repeat (1, 1)); // func and args
  init (INCLUDE, "include", fixed (1));

  init (OR, "or", repeat (2, 1));
  init (XOR, "xor", fixed (2));
  init (AND, "and", repeat (2, 1));
  init (NOT, "not", fixed (1));
  init (PLUS, "plus", fixed (2));
  init (MINUS, "minus", fixed (2));
  init (TIMES, "times", fixed (2));
  init (OVER, "over", fixed (2));
  init (DIV, "div", fixed (2) -> name ("divide"));
  init (MOD, "mod", fixed (2) -> name ("modulo"));
  init (MERGE, "merge", fixed (2));
  init (LENGTH, "length", fixed (1));
  init (RANGE, "range", fixed (3));
  init (NUMBER, "number", fixed (2));
  init (_DATE, "date", options (0, 2));
  init (TRANSLATE, "translate", fixed (3));
  init (FIND_FILE, "find_file", var_repeat (1, 1)); // dirs and file
  init (IS_TUPLE, "is_tuple", fixed (1) -> name ("tuple?"));
  init (LOOK_UP, "look_up", fixed (2));
  init (EQUAL, "equal", fixed (2));
  init (UNEQUAL, "unequal", fixed (2) -> name ("not equal"));
  init (LESS, "less", fixed (2));
  init (LESSEQ, "lesseq", fixed (2) -> name ("less or equal"));
  init (GREATER, "greater", fixed (2));
  init (GREATEREQ, "greatereq", fixed (2) -> name ("greater or equal"));
  init (IF, "if", options (2, 1));
  init (VAR_IF, "var_if", fixed (2));
  init (CASE, "case", repeat (2, 1));
  init (WHILE, "while", fixed (2));

  init (INACTIVE, "inactive", fixed (1) -> accessible (0));
  init (ACTIVE, "active", fixed (1) -> accessible (0));
  init (VAR_INACTIVE, "var_inactive", fixed (1) -> accessible (0));
  init (VAR_ACTIVE, "var_active", fixed (1) -> accessible (0));
  init (SYMBOL, "symbol", fixed (1));
  init (LATEX, "latex", fixed (1));
  init (HYBRID, "hybrid", options (1, 1));
  init (TUPLE, "tuple", repeat (0, 1));
  init (ATTR, "attr", repeat (2, 2) -> accessible (0) -> name ("attributes"));
  init (COLLECTION, "collection", repeat (1, 1));
  init (ASSOCIATE, "associate", fixed (2));
  init (BACKUP, "backup", fixed (2));
  init (LABEL, "label", fixed (1));
  init (REFERENCE, "reference", fixed (1));
  init (PAGEREF, "pageref", fixed (1) -> name ("page reference"));
  init (WRITE, "write", fixed (2));
  init (SPECIFIC, "specific", fixed (2));
  init (HLINK, "hlink",
	fixed (1, 1, BIFORM) -> accessible (0) -> name ("hyperlink"));
  init (ACTION, "action", options (2, 1, DETAILED) -> accessible (0));
  init (TAG, "tag", fixed (1, 1, BIFORM) -> accessible (0));
  init (MEANING, "meaning", fixed (1, 1, BIFORM) -> accessible (0));
  init (FLAG, "flag", options (2, 1));

  init (GRAPHICS, "graphics", repeat (1, 1) -> accessible (0));
  init (SUPERPOSE, "superpose", repeat (1, 1));
  init (TEXT_AT, "text_at", fixed (1, 3, BIFORM) -> accessible (0));
  init (_POINT, "point", repeat (1, 1));
  init (LINE, "line", repeat (2, 1));
  init (CLINE, "cline", repeat (3, 1));
  init (SPLINE, "spline", repeat (2, 1));
  init (VAR_SPLINE, "var_spline", repeat (2, 1));
  init (CSPLINE, "cspline", repeat (2, 1));
  init (FILL, "fill", repeat (1, 1));
  init (POSTSCRIPT, "postscript", fixed (7));

  init (FORMAT, "format", repeat (1, 1));
  init (SPLIT, "split", repeat (1, 1));
  init (OLD_MATRIX, "old_matrix", var_repeat (1, 2, BIFORM) -> accessible (0));
  init (OLD_TABLE, "old_table", var_repeat (1, 2, BIFORM) -> accessible (0));
  init (OLD_MOSAIC, "old_mosaic", var_repeat (1, 2, BIFORM) -> accessible (0));
  init (OLD_MOSAIC_ITEM, "old_mosaic_item", repeat (1, 1) -> accessible (0));
  init (SET, "set", fixed (2));
  init (RESET, "reset", fixed (1));
  init (EXPAND, "expand", repeat (1, 1, BIFORM) -> accessible (1));
  init (VAR_EXPAND, "var_expand",
	repeat (1, 1, BIFORM) -> no_border () -> accessible (1));
  init (HIDE_EXPAND, "hide_expand", repeat (2, 1, DETAILED) -> accessible (1));
  init (APPLY, "apply", repeat (1, 1));
  init (BEGIN, "begin", repeat (1, 1));
  init (END, "end", fixed (1));
  init (FUNC, "func", var_repeat (1, 1));
  init (ENV, "env", var_repeat (1, 2));
  init (AUTHORIZE, "authorize", fixed (2));
}
