
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
  //cout << "l= " << l << ", " << name << ", " << ti << "\n";
  //cout << "Check name...\n";
  if (as_string (l) != name)
    cout << name << ": Bad name (" << as_string (l) << ")\n";
  tag_info std= std_drd->info[l];

  //cout << "Check arity...\n";
  bool arity_ok;
  if (ti->pi.arity_mode != ARITY_NORMAL) arity_ok= (std->arity == -1);
  else arity_ok= (std->arity ==
		  (((int) ti->pi.arity_base) + ((int) ti->pi.arity_extra)));
  if (!arity_ok)
    cout << name << ": Bad arity (" << std->arity << ")\n";

  //cout << "Check no_border...\n";
  if (ti->pi.no_border != ((std->props & BORDER_ACCESSIBLE_MASK)>0))
    cout << name << ": Bad no_border ("
	 << ((std->props & BORDER_ACCESSIBLE_MASK)>0) << ")\n";

  //cout << "Check dynamic...\n";
  /*
  bool dyn1= (std->props & DYNAMIC_MASK) > 0;
  bool dyn2= ti->pi.arity_mode != ARITY_NORMAL;
  if ((dyn2 != dyn1) && (std->arity <= 0))
    cout << name << ": Bad dynamic ("
	 << ((std->props & DYNAMIC_MASK)>0) << ")\n";
  */

  //cout << "Check accessible...\n";
  array<child_info>& ci= ti->ci;
  int i, n= N(ci);
  bool access_ok= true;
  switch (std->props & ACCESSIBLE_MASK) {
  case NOT_ACCESSIBLE:
    for (i=0; i<n; i++)
      access_ok= access_ok && (ci[i].accessible == false);
    break;
  case ACCESSIBLE:
    for (i=0; i<n; i++)
      access_ok= access_ok && (ci[i].accessible == true);
    break;
  case FIRST_ACCESSIBLE:
    if (ti->pi.child_mode != CHILD_DETAILED) {
      access_ok= access_ok && (ti->pi.child_mode != CHILD_UNIFORM);
      access_ok= access_ok && (ti->pi.arity_mode != ARITY_VAR_REPEAT);
      access_ok= access_ok && (ti->pi.arity_base == 1);
    }
    access_ok= access_ok && (ci[0].accessible == true);
    for (i=1; i<n; i++)
      access_ok= access_ok && (ci[i].accessible == false);
    break;
  case LAST_ACCESSIBLE:
    if (ti->pi.child_mode != CHILD_DETAILED) {
      access_ok= access_ok && (ti->pi.child_mode != CHILD_UNIFORM);
      if (ti->pi.arity_mode == ARITY_NORMAL)
	access_ok= access_ok && (ti->pi.arity_extra == 1);
      else if (ti->pi.arity_mode == ARITY_VAR_REPEAT)
	access_ok= access_ok && (ti->pi.arity_base == 1);
      else access_ok= false;
    }
    access_ok= access_ok && (ci[n-1].accessible == true);
    for (i=0; i<n-1; i++)
      access_ok= access_ok && (ci[i].accessible == false);
    break;
  case TAIL_ACCESSIBLE:
    if (ti->pi.child_mode != CHILD_DETAILED) {
      access_ok= access_ok && (ti->pi.child_mode != CHILD_UNIFORM);
      access_ok= access_ok && (ti->pi.arity_mode != ARITY_VAR_REPEAT);
      access_ok= access_ok && (ti->pi.arity_base == 1);
    }
    access_ok= access_ok && (ci[0].accessible == false);
    for (i=1; i<n; i++)
      access_ok= access_ok && (ci[i].accessible == true);
    break;
  }
  if (!access_ok)
    cout << name << ": Bad accessability ("
	 << (std->props & ACCESSIBLE_MASK) << ")\n";

  std->pi= ti->pi;
  std->ci= ti->ci;
}

static bool std_drd_initialized= false;

void
initialize_std_drd () {
  if (std_drd_initialized) return;
  std_drd_initialized=true;

  constructor (STRING, "string", 0);
  constructor (UNKNOWN, "unknown", 0);
  constructor (UNINIT, "uninit", 0);
  constructor (ERROR, "error", 1);
  constructor (RAW_DATA, "raw_data", 1);

  constructor (DOCUMENT, "document", -1, ACCESSIBLE_EXCEPT_BORDER);
  constructor (PARAGRAPH, "para", -1, ACCESSIBLE_EXCEPT_BORDER);
  constructor (SURROUND, "surround", 3, ACCESSIBLE);
  constructor (CONCAT, "concat", -1, ACCESSIBLE_EXCEPT_BORDER);
  constructor (GROUP, "group", 1, ACCESSIBLE);
  constructor (HSPACE, "hspace", -1);
  constructor (VSPACE_BEFORE, "vspace*", -1);
  constructor (VSPACE_AFTER, "vspace", -1);
  constructor (SPACE, "space", -1);
  constructor (HTAB, "htab", -1, DYNAMIC);
  constructor (MOVE, "move", 3, FIRST_ACCESSIBLE);
  constructor (RESIZE, "resize", 5, FIRST_ACCESSIBLE);
  constructor (REPEAT, "repeat", 2, FIRST_ACCESSIBLE);
  constructor (_FLOAT, "float", 3, LAST_ACCESSIBLE);
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

  constructor (TABLE_FORMAT, "tformat", -1, ONLY_LAST_ACCESSIBLE + DYNAMIC);
  constructor (TABLE_WITH, "twith", 2, ACCESSIBLE);
  constructor (CELL_WITH, "cwith", 6, ACCESSIBLE);
  constructor (TABLE_MARKER, "tmarker", 0);
  constructor (TABLE, "table", -1, ACCESSIBLE_EXCEPT_BORDER);
  constructor (ROW, "row", -1, ACCESSIBLE_EXCEPT_BORDER);
  constructor (CELL, "cell", 1, ACCESSIBLE_EXCEPT_BORDER);
  constructor (SUB_TABLE, "sub_table", 1, ACCESSIBLE_EXCEPT_BORDER);

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

  constructor (ASSIGN, "assign", 2, DYNAMIC);
  constructor (WITH, "with", -1, LAST_ACCESSIBLE + DYNAMIC);
  constructor (VALUE, "value", 1, DYNAMIC);
  constructor (MACRO, "macro", -1, ACCESSIBLE + DYNAMIC);
  constructor (DRD_PROPS, "drd_props", -1, DYNAMIC);
  constructor (ARGUMENT, "arg", -1, DYNAMIC);
  constructor (COMPOUND, "compound", -1, TAIL_ACCESSIBLE + DYNAMIC);
  constructor (XMACRO, "xmacro", 2, ACCESSIBLE + DYNAMIC);
  constructor (GET_LABEL, "get_label", 1, DYNAMIC);
  constructor (GET_ARITY, "get_arity", 1, DYNAMIC);
  constructor (MAP_ARGS, "map_args", -1, DYNAMIC);
  constructor (EVAL_ARGS, "eval_args", 1, DYNAMIC);
  constructor (EVAL, "eval", 1, DYNAMIC);
  constructor (QUOTE, "quote", 1, DYNAMIC);
  constructor (DELAY, "delay", 1, DYNAMIC);
  constructor (HOLD, "hold", 1, DYNAMIC);
  constructor (RELEASE, "release", 1, DYNAMIC);
  constructor (EXTERN, "extern", -1, DYNAMIC);
  constructor (INCLUDE, "include", 1, DYNAMIC);

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

  constructor (INACTIVE, "inactive", 1, ACCESSIBLE);
  constructor (ACTIVE, "active", 1, ACCESSIBLE);
  constructor (VAR_INACTIVE, "var_inactive", 1, ACCESSIBLE);
  constructor (VAR_ACTIVE, "var_active", 1, ACCESSIBLE);
  constructor (SYMBOL, "symbol", 1);
  constructor (LATEX, "latex", 1, ACCESSIBLE + DYNAMIC);
  constructor (HYBRID, "hybrid", -1, ACCESSIBLE + DYNAMIC);
  constructor (TUPLE, "tuple", -1, DYNAMIC);
  constructor (ATTR, "attr", -1, ACCESSIBLE + DYNAMIC);
  constructor (COLLECTION, "collection", -1);
  constructor (ASSOCIATE, "associate", 2);
  constructor (BACKUP, "backup", 2);
  constructor (LABEL, "label", 1, DYNAMIC);
  constructor (REFERENCE, "reference", 1, DYNAMIC);
  constructor (PAGEREF, "pageref", 1, DYNAMIC);
  constructor (WRITE, "write", 2, DYNAMIC);
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
  constructor (POSTSCRIPT, "postscript", 7, DYNAMIC);

  constructor (FORMAT, "format", -1);
  constructor (SPLIT, "split", -1, DYNAMIC);
  constructor (OLD_MATRIX, "old_matrix", -1, TABLE_ACCESSIBLE);
  constructor (OLD_TABLE, "old_table", -1, TABLE_ACCESSIBLE);
  constructor (OLD_MOSAIC, "old_mosaic", -1, TABLE_ACCESSIBLE);
  constructor (OLD_MOSAIC_ITEM, "old_mosaic_item", -1, ACCESSIBLE);
  constructor (SET, "set", 2, DYNAMIC);
  constructor (RESET, "reset", 1, DYNAMIC);
  constructor (EXPAND, "expand", -1, TAIL_ACCESSIBLE + DYNAMIC);
  constructor (VAR_EXPAND, "var_expand", -1,
	       TAIL_ACCESSIBLE + DYNAMIC + BORDER_NOT_ACCESSIBLE);
  constructor (HIDE_EXPAND, "hide_expand", -1,
	       HIDE_EXPAND_ACCESSIBLE + DYNAMIC);
  constructor (APPLY, "apply", -1, DYNAMIC);
  constructor (BEGIN, "begin", -1, DYNAMIC);
  constructor (END, "end", 1, DYNAMIC);
  constructor (FUNCTION, "func", -1, DYNAMIC);
  constructor (ENVIRONMENT, "env", -1, DYNAMIC);
  constructor (PROVIDES, "provides", 1, DYNAMIC);
  constructor (AUTHORIZE, "authorize", 2);

  init (STRING, "string", fixed (0));
  init (UNKNOWN, "unknown", fixed (0));
  init (UNINIT, "uninit", fixed (0));
  init (ERROR, "error", fixed (1));
  init (RAW_DATA, "raw_data", fixed (1));

  init (DOCUMENT, "document", repeat (1, 1) -> no_border () -> accessible (0));
  init (PARAGRAPH, "para", repeat (1, 1) -> no_border () -> accessible (0));
  init (SURROUND, "surround", fixed (3) -> accessible (0));
  init (CONCAT, "concat", repeat (1, 1) -> no_border () -> accessible (0));
  init (GROUP, "group", fixed (1) -> accessible (0));
  init (HSPACE, "hspace", options (1, 2)); // arity 1 or 3
  init (VSPACE_BEFORE, "vspace*", options (1, 2)); // arity 1 or 3
  init (VSPACE_AFTER, "vspace", options (1, 2)); // arity 1 or 3
  init (SPACE, "space", options (1, 2)); // arity 1 or 3
  init (HTAB, "htab", options (1, 1));
  init (MOVE, "move", fixed (1, 2, BIFORM) -> accessible (0));
  init (RESIZE, "resize", fixed (1, 4, BIFORM) -> accessible (0));
  init (REPEAT, "repeat", fixed (1, 1, BIFORM) -> accessible (0));
  init (_FLOAT, "float", fixed (2, 1, BIFORM) -> accessible (1));
  init (DECORATE_ATOMS, "datoms", var_repeat (1, 1, BIFORM) -> accessible (1));
  // arbitrary number of macros and decorated content
  init (DECORATE_LINES, "dlines", var_repeat (1, 1, BIFORM) -> accessible (1));
  init (DECORATE_PAGES, "dpages", var_repeat (1, 1, BIFORM) -> accessible (1));
  init (DECORATED_BOX, "dbox", fixed (0));

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

  init (TABLE_FORMAT, "tformat",
	var_repeat (1, 1, BIFORM) -> no_border () -> accessible (1));
  init (TABLE_WITH, "twith", fixed (2) -> accessible (0));
  init (CELL_WITH, "cwith", fixed (6) -> accessible (0));
  init (TABLE_MARKER, "tmarker", fixed (0));
  init (TABLE, "table", repeat (1, 1) -> no_border () -> accessible (0));
  init (ROW, "row", repeat (1, 1) -> no_border () -> accessible (0));
  init (CELL, "cell", fixed (1) -> no_border () -> accessible (0));
  init (SUB_TABLE, "sub_table", fixed (1) -> no_border () -> accessible (0));

  init (LEFT, "left", fixed (1));
  init (MIDDLE, "mid", fixed (1));
  init (RIGHT, "right", fixed (1));
  init (BIG, "big", fixed (1));
  init (LEFT_PRIME, "lprime", fixed (1));
  init (RIGHT_PRIME, "rprime", fixed (1));
  init (BELOW, "below", fixed (2) -> accessible (0));
  init (ABOVE, "above", fixed (2) -> accessible (0));
  init (LEFT_SUB, "lsub", fixed (1) -> accessible (0));
  init (LEFT_SUP, "lsup", fixed (1) -> accessible (0));
  init (RIGHT_SUB, "rsub", fixed (1) -> accessible (0));
  init (RIGHT_SUP, "rsup", fixed (1) -> accessible (0));
  init (FRAC, "frac", fixed (2) -> accessible (0));
  init (SQRT, "sqrt", options (1, 1) -> accessible (0));
  init (WIDE, "wide", fixed (1) -> accessible (0));
  init (WIDE_UNDER, "wide*", fixed (1) -> accessible (0));
  init (NEG, "neg", fixed (1) -> accessible (0));
  init (TREE, "tree", repeat (2, 1) -> accessible (0));

  init (ASSIGN, "assign", fixed (2));
  init (WITH, "with", var_repeat (2, 1, BIFORM) -> accessible (1));
  init (VALUE, "value", fixed (1));
  init (MACRO, "macro", var_repeat (1, 1) -> accessible (0));
  init (DRD_PROPS, "drd_props", repeat (3, 2));
  init (ARGUMENT, "arg", repeat (1, 1));
  init (COMPOUND, "compound", repeat (1, 1, BIFORM) -> accessible (1));
  init (XMACRO, "xmacro", fixed (2) -> accessible (0));
  init (GET_LABEL, "get_label", fixed (1));
  init (GET_ARITY, "get_arity", fixed (1));
  init (MAP_ARGS, "map_args", options (3, 2));
  init (EVAL_ARGS, "eval_args", fixed (1));
  init (EVAL, "eval", fixed (1));
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
  init (DIVIDE, "div", fixed (2));
  init (MODULO, "mod", fixed (2));
  init (MERGE, "merge", fixed (2));
  init (LENGTH, "length", fixed (1));
  init (RANGE, "range", fixed (3));
  init (NUMBER, "number", fixed (2));
  init (_DATE, "date", options (0, 2));
  init (TRANSLATE, "translate", fixed (3));
  init (FIND_FILE, "find_file", var_repeat (1, 1)); // dirs and file
  init (IS_TUPLE, "is_tuple", fixed (1));
  init (LOOK_UP, "look_up", fixed (2));
  init (EQUAL, "equal", fixed (2));
  init (UNEQUAL, "unequal", fixed (2));
  init (LESS, "less", fixed (2));
  init (LESSEQ, "lesseq", fixed (2));
  init (GREATER, "greater", fixed (2));
  init (GREATEREQ, "greatereq", fixed (2));
  init (IF, "if", options (1, 1));
  init (VAR_IF, "var_if", fixed (2));
  init (CASE, "case", repeat (1, 1));
  init (WHILE, "while", fixed (2));

  init (INACTIVE, "inactive", fixed (1) -> accessible (0));
  init (ACTIVE, "active", fixed (1) -> accessible (0));
  init (VAR_INACTIVE, "var_inactive", fixed (1) -> accessible (0));
  init (VAR_ACTIVE, "var_active", fixed (1) -> accessible (0));
  init (SYMBOL, "symbol", fixed (1));
  init (LATEX, "latex", fixed (1) -> accessible (0));
  init (HYBRID, "hybrid", options (1, 1) -> accessible (0));
  init (TUPLE, "tuple", repeat (0, 1));
  init (ATTR, "attr", repeat (2, 2) -> accessible (0));
  init (COLLECTION, "collection", repeat (1, 1));
  init (ASSOCIATE, "associate", fixed (2));
  init (BACKUP, "backup", fixed (2));
  init (LABEL, "label", fixed (1));
  init (REFERENCE, "reference", fixed (1));
  init (PAGEREF, "pageref", fixed (1));
  init (WRITE, "write", fixed (2));
  init (SPECIFIC, "specific", fixed (2));
  init (HYPERLINK, "hlink", fixed (1, 1, BIFORM) -> accessible (0));
  init (ACTION, "action", options (2, 1, DETAILED) -> accessible (0));
  init (TAG, "tag", fixed (1, 1, BIFORM) -> accessible (0));
  init (MEANING, "meaning", fixed (1, 1, BIFORM) -> accessible (0));

  init (GRAPHICS, "graphics", repeat (1, 1));
  init (SUPERPOSE, "superpose", repeat (1, 1));
  init (TEXT_AT, "text_at", fixed (1, 3, BIFORM) -> accessible (0));
  init (_POINT, "point", repeat (1, 1));
  init (LINE, "line", repeat (1, 1));
  init (CLINE, "cline", repeat (1, 1));
  init (SPLINE, "spline", repeat (1, 1));
  init (VAR_SPLINE, "var_spline", repeat (1, 1));
  init (CSPLINE, "cspline", repeat (1, 1));
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
  init (FUNCTION, "func", var_repeat (1, 1));
  init (ENVIRONMENT, "env", var_repeat (1, 2));
  init (PROVIDES, "provides", fixed (1));
  init (AUTHORIZE, "authorize", fixed (2));
}
