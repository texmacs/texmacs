
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

static void
constructor (tree_label l, string name, int arity, int properties= 0) {
  STD_CODE(name)= (int) l;
  make_tree_label (l, name);
  std_drd->set_arity (l, arity);
  std_drd->set_props (l, properties);
}

static bool std_drd_initialized= false;

void
initialize_std_drd () {
  if (std_drd_initialized) return;
  std_drd_initialized=true;

  constructor (UNKNOWN, "unknown", 0);
  constructor (UNINIT, "uninit", 0);
  constructor (ERROR, "error", 1);
  constructor (RAW_DATA, "raw_data", 1);

  constructor (DOCUMENT, "document", -1, ACCESSIBLE_EXCEPT_BORDER);
  constructor (PARAGRAPH, "para", -1, ACCESSIBLE_EXCEPT_BORDER);
  constructor (SURROUND, "surround", 3, ACCESSIBLE + DYNAMIC);
  constructor (CONCAT, "concat", -1, ACCESSIBLE_EXCEPT_BORDER);
  constructor (FORMAT, "format", -1);
  constructor (HSPACE, "hspace", -1);
  constructor (VSPACE_BEFORE, "vspace*", -1);
  constructor (VSPACE_AFTER, "vspace", -1);
  constructor (SPACE, "space", -1);
  constructor (HTAB, "htab", -1, DYNAMIC);
  constructor (SPLIT, "split", -1, DYNAMIC);
  constructor (MOVE, "move", 3, FIRST_ACCESSIBLE + DYNAMIC);
  constructor (RESIZE, "resize", 5, FIRST_ACCESSIBLE + DYNAMIC);
  constructor (_FLOAT, "float", 3, LAST_ACCESSIBLE + DYNAMIC);
  constructor (REPEAT, "repeat", 2, FIRST_ACCESSIBLE + DYNAMIC);
  constructor (DECORATE_ATOMS, "datoms", -1, LAST_ACCESSIBLE + DYNAMIC);
  constructor (DECORATE_LINES, "dlines", -1, LAST_ACCESSIBLE + DYNAMIC);
  constructor (DECORATE_PAGES, "dpages", -1, LAST_ACCESSIBLE + DYNAMIC);
  constructor (DECORATED_BOX, "dbox", 0);

  constructor (WITH_LIMITS, "with_limits", 0);
  constructor (LINE_BREAK, "line_break", 0);
  constructor (NEW_LINE, "new_line", 0);
  constructor (LINE_SEP, "line_separator", 0);
  constructor (NEXT_LINE, "next_line", 0);
  constructor (NO_BREAK, "no_line_break", 0);
  constructor (NO_FIRST_INDENT, "no_first_indentation", 0);
  constructor (YES_FIRST_INDENT, "enable_first_indentation", 0);
  constructor (NO_FIRST_INDENT_AFTER, "no_indentation_after", 0);
  constructor (YES_FIRST_INDENT_AFTER, "enable_indentation_after", 0);
  constructor (PAGE_BREAK_BEFORE, "page_break_before", 0);
  constructor (PAGE_BREAK, "page_break", 0);
  constructor (NO_PAGE_BREAK_BEFORE, "no_page_break_before", 0);
  constructor (NO_PAGE_BREAK_AFTER, "no_page_break_after", 0);
  constructor (NEW_PAGE_BEFORE, "new_page_before", 0);
  constructor (NEW_PAGE, "new_page", 0);
  constructor (NEW_DOUBLE_PAGE_BEFORE, "new_double_page_before", 0);
  constructor (NEW_DOUBLE_PAGE, "new_double_page", 0);

  constructor (GROUP, "group", 1, ACCESSIBLE);
  constructor (LEFT, "left", 1);
  constructor (MIDDLE, "mid", 1);
  constructor (RIGHT, "right", 1);
  constructor (BIG, "big", 1);
  constructor (LEFT_PRIME, "lprime", 1);
  constructor (RIGHT_PRIME, "rprime", 1);
  constructor (BELOW, "below", 2, ACCESSIBLE);
  constructor (ABOVE, "above", 2, ACCESSIBLE);
  constructor (LEFT_SUB, "lsub", 1, ACCESSIBLE);
  constructor (LEFT_SUP, "lsup", 1, ACCESSIBLE);
  constructor (RIGHT_SUB, "rsub", 1, ACCESSIBLE);
  constructor (RIGHT_SUP, "rsup", 1, ACCESSIBLE);
  constructor (FRAC, "frac", 2, ACCESSIBLE);
  constructor (SQRT, "sqrt", -1, ACCESSIBLE);
  constructor (WIDE, "wide", 1, ACCESSIBLE);
  constructor (WIDE_UNDER, "wide*", 1, ACCESSIBLE);
  constructor (NEG, "neg", 1, ACCESSIBLE);
  constructor (TREE, "tree", -1, ACCESSIBLE);
  constructor (OLD_MATRIX, "old_matrix", -1, TABLE_ACCESSIBLE);
  constructor (OLD_TABLE, "old_table", -1, TABLE_ACCESSIBLE);
  constructor (OLD_MOSAIC, "old_mosaic", -1, TABLE_ACCESSIBLE);
  constructor (OLD_MOSAIC_ITEM, "old_mosaic_item", -1, ACCESSIBLE);

  constructor (TABLE_FORMAT, "tformat", -1, ONLY_LAST_ACCESSIBLE + DYNAMIC);
  constructor (TABLE_WITH, "twith", 2, ACCESSIBLE);
  constructor (CELL_WITH, "cwith", 6, ACCESSIBLE);
  constructor (TABLE_MARKER, "tmarker", 0);
  constructor (TABLE, "table", -1, ACCESSIBLE_EXCEPT_BORDER);
  constructor (ROW, "row", -1, ACCESSIBLE_EXCEPT_BORDER);
  constructor (CELL, "cell", 1, ACCESSIBLE_EXCEPT_BORDER);
  constructor (SUB_TABLE, "sub_table", 1, ACCESSIBLE_EXCEPT_BORDER);

  constructor (ASSIGN, "assign", 2, DYNAMIC);
  constructor (WITH, "with", -1, LAST_ACCESSIBLE + DYNAMIC);
  constructor (SET, "set", 2, DYNAMIC);
  constructor (RESET, "reset", 1, DYNAMIC);
  constructor (EXPAND, "expand", -1, TAIL_ACCESSIBLE + DYNAMIC);
  constructor (VAR_EXPAND, "var_expand", -1,
	       TAIL_ACCESSIBLE + DYNAMIC + BORDER_NOT_ACCESSIBLE);
  constructor (HIDE_EXPAND, "hide_expand", -1,
	       HIDE_EXPAND_ACCESSIBLE + DYNAMIC);
  constructor (COMPOUND, "compound", -1, TAIL_ACCESSIBLE + DYNAMIC);
  constructor (APPLY, "apply", -1, DYNAMIC);
  constructor (BEGIN, "begin", -1, DYNAMIC);
  constructor (END, "end", 1, DYNAMIC);
  constructor (INCLUDE, "include", 1, DYNAMIC);
  constructor (MACRO, "macro", -1, DYNAMIC);
  constructor (XMACRO, "xmacro", 2, DYNAMIC);
  constructor (FUNCTION, "func", -1, DYNAMIC);
  constructor (ENVIRONMENT, "env", -1, DYNAMIC);
  constructor (DRD_PROPS, "drd_props", -1, DYNAMIC);
  constructor (EVAL, "eval", 1, DYNAMIC);
  constructor (PROVIDES, "provides", 1, DYNAMIC);
  constructor (VALUE, "value", 1, DYNAMIC);
  constructor (ARGUMENT, "arg", -1, DYNAMIC);
  constructor (GET_LABEL, "get_label", 1, DYNAMIC);
  constructor (GET_ARITY, "get_arity", 1, DYNAMIC);
  constructor (BACKUP, "backup", 1);
  constructor (QUOTE, "quote", 1, DYNAMIC);
  constructor (DELAY, "delay", 1, DYNAMIC);
  constructor (HOLD, "hold", 1, DYNAMIC);
  constructor (RELEASE, "release", 1, DYNAMIC);

  constructor (OR, "or", -1, DYNAMIC);
  constructor (XOR, "xor", 2);
  constructor (AND, "and", -1, DYNAMIC);
  constructor (NOT, "not", 1);
  constructor (PLUS, "plus", 2);
  constructor (MINUS, "minus", 2);
  constructor (TIMES, "times", 2);
  constructor (OVER, "over", 2);
  constructor (DIVIDE, "div", 2);
  constructor (MODULO, "mod", 2);
  constructor (MERGE, "merge", 2);
  constructor (LENGTH, "length", 1);
  constructor (RANGE, "range", 3);
  constructor (NUMBER, "number", 2);
  constructor (_DATE, "date", -1);
  constructor (TRANSLATE, "translate", 3);
  constructor (FIND_FILE, "find_file", -1, DYNAMIC);
  constructor (IS_TUPLE, "is_tuple", 1);
  constructor (LOOK_UP, "look_up", 2);
  constructor (EQUAL, "equal", 2);
  constructor (UNEQUAL, "unequal", 2);
  constructor (LESS, "less", 2);
  constructor (LESSEQ, "lesseq", 2);
  constructor (GREATER, "greater", 2);
  constructor (GREATEREQ, "greatereq", 2);
  constructor (IF, "if", -1, DYNAMIC);
  constructor (VAR_IF, "var_if", 2, DYNAMIC);
  constructor (CASE, "case", -1, DYNAMIC);
  constructor (WHILE, "while", 2);
  constructor (EXTERN, "extern", -1, DYNAMIC);
  constructor (AUTHORIZE, "authorize", 2);

  constructor (INACTIVE, "inactive", 1, ACCESSIBLE);
  constructor (ACTIVE, "active", 1, ACCESSIBLE);
  constructor (VAR_INACTIVE, "var_inactive", 1, ACCESSIBLE);
  constructor (VAR_ACTIVE, "var_active", 1, ACCESSIBLE);
  constructor (SYMBOL, "symbol", 1);
  constructor (LATEX, "latex", 1, DYNAMIC);
  constructor (HYBRID, "hybrid", 1, DYNAMIC);
  constructor (TUPLE, "tuple", -1, DYNAMIC);
  constructor (ATTR, "attr", -1, DYNAMIC);
  constructor (COLLECTION, "collection", -1);
  constructor (ASSOCIATE, "associate", 2);
  constructor (LABEL, "label", -1, DYNAMIC);
  constructor (REFERENCE, "reference", -1, DYNAMIC);
  constructor (PAGEREF, "pageref", -1, DYNAMIC);
  constructor (WRITE, "write", -1, DYNAMIC);
  constructor (SPECIFIC, "specific", 2, DYNAMIC + NOT_ACCESSIBLE);
  constructor (HYPERLINK, "hlink", 2, FIRST_ACCESSIBLE + DYNAMIC);
  constructor (ACTION, "action", -1, FIRST_ACCESSIBLE + DYNAMIC);
  constructor (TAG, "tag", 2, FIRST_ACCESSIBLE + DYNAMIC);
  constructor (MEANING, "meaning", 2, FIRST_ACCESSIBLE + DYNAMIC);

  constructor (GRAPHICS, "graphics", -1, DYNAMIC);
  constructor (SUPERPOSE, "superpose", -1, DYNAMIC);
  constructor (TEXT_AT, "text_at", 4, DYNAMIC + FIRST_ACCESSIBLE);
  constructor (_POINT, "point", -1, DYNAMIC + NOT_ACCESSIBLE);
  constructor (LINE, "line", -1, DYNAMIC + NOT_ACCESSIBLE);
  constructor (CLINE, "cline", -1, DYNAMIC + NOT_ACCESSIBLE);
  constructor (SPLINE, "spline", -1, DYNAMIC + NOT_ACCESSIBLE);
  constructor (VAR_SPLINE, "var_spline", -1, DYNAMIC + NOT_ACCESSIBLE);
  constructor (CSPLINE, "cspline", -1, DYNAMIC + NOT_ACCESSIBLE);
  constructor (FILL, "fill", -1, DYNAMIC);
  constructor (POSTSCRIPT, "postscript", -1, DYNAMIC);
}
