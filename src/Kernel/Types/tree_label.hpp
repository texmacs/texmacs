
/******************************************************************************
* MODULE     : tree_label.hpp
* DESCRIPTION: labels of trees
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef TREE_LABEL_H
#define TREE_LABEL_H
#include "string.hpp"

/******************************************************************************
* Standard tree labels
******************************************************************************/

enum tree_label {
  STRING= 0, UNKNOWN, UNINIT, ERROR, RAW_DATA,

  // basic formatting tags
  DOCUMENT, PARA, SURROUND, CONCAT,
  RIGID, HIDDEN, FREEZE, UNFREEZE,
  HSPACE, VAR_VSPACE, VSPACE, SPACE, HTAB,
  MOVE, SHIFT, RESIZE, CLIPPED,
  REPEAT, _FLOAT, DATOMS, DLINES, DPAGES, DBOX,

  // zero-ary formatting directives
  // modify is_formatting predicate when inserting new tags
  WITH_LIMITS, LINE_BREAK, NEW_LINE, NEXT_LINE,
  NO_BREAK, YES_INDENT, NO_INDENT, VAR_YES_INDENT,
  VAR_NO_INDENT, VAR_PAGE_BREAK, PAGE_BREAK,
  VAR_NO_PAGE_BREAK, NO_PAGE_BREAK, VAR_NEW_PAGE,
  NEW_PAGE, VAR_NEW_DPAGE, NEW_DPAGE,

  // mathematics
  AROUND, VAR_AROUND, BIG_AROUND,
  LEFT, MID, RIGHT, BIG,
  LPRIME, RPRIME, BELOW, ABOVE,
  LSUB, LSUP, RSUB, RSUP,
  FRAC, SQRT, WIDE, VAR_WIDE, NEG, TREE,

  // tabular material
  TFORMAT, TWITH, CWITH, TMARKER,
  TABLE, ROW, CELL, SUBTABLE,

  // macro language
  ASSIGN, WITH, PROVIDES, VALUE, QUOTE_VALUE,
  MACRO, DRD_PROPS, ARG, QUOTE_ARG, COMPOUND,
  XMACRO, GET_LABEL, GET_ARITY, MAP_ARGS, EVAL_ARGS,
  MARK, EXPAND_AS,
  EVAL, QUOTE, QUASI, QUASIQUOTE, UNQUOTE, VAR_UNQUOTE, COPY,
  IF, VAR_IF, CASE, WHILE, FOR_EACH,
  EXTERN, INCLUDE, USE_PACKAGE, USE_MODULE,

  // computational markup
  OR, XOR, AND, NOT,
  PLUS, MINUS, TIMES, OVER, DIV, MOD, MINIMUM, MAXIMUM,
  MATH_SQRT, EXP, LOG, POW, COS, SIN, TAN,
  MERGE, LENGTH, RANGE, NUMBER, _DATE, TRANSLATE, CHANGE_CASE, FIND_FILE,
  IS_TUPLE, LOOK_UP,
  EQUAL, UNEQUAL, LESS, LESSEQ, GREATER, GREATEREQ,

  // built-in length units
  CM_LENGTH, MM_LENGTH, IN_LENGTH, PT_LENGTH,
  BP_LENGTH, DD_LENGTH, PC_LENGTH, CC_LENGTH,     // standard absolute units
  FS_LENGTH, FBS_LENGTH, EM_LENGTH,
  LN_LENGTH, SEP_LENGTH, YFRAC_LENGTH, EX_LENGTH, // font specific
  FN_LENGTH, FNS_LENGTH, BLS_LENGTH,
  FNBOT_LENGTH, FNTOP_LENGTH,
  SPC_LENGTH, XSPC_LENGTH,                        // font specific & stretchy
  PAR_LENGTH, PAG_LENGTH,                         // other
  GW_LENGTH, GH_LENGTH,                           // width & height of graphics
  TMPT_LENGTH, PX_LENGTH,                         // miscellaneous
  MSEC_LENGTH, SEC_LENGTH, MIN_LENGTH, HR_LENGTH,  // durations

  // tags for source tree editing
  STYLE_WITH, VAR_STYLE_WITH, STYLE_ONLY, VAR_STYLE_ONLY,
  ACTIVE, VAR_ACTIVE, INACTIVE, VAR_INACTIVE,
  REWRITE_INACTIVE, INLINE_TAG, OPEN_TAG, MIDDLE_TAG, CLOSE_TAG,
  SYMBOL, LATEX, HYBRID,

  // linking primitives
  LOCUS, ID, HARD_ID,
  LINK, URL, SCRIPT, HLINK, ACTION,
  SET_BINDING, GET_BINDING, LABEL, REFERENCE, PAGEREF, WRITE,

  // various other tags
  TUPLE, ATTR, TMLEN, COLLECTION, ASSOCIATE, BACKUP, PATTERN, GRADIENT,
  SPECIFIC, FLAG,

  // animations
  ANIM_COMPOSE, ANIM_REPEAT, ANIM_CONSTANT,
  ANIM_TRANSLATE, ANIM_PROGRESSIVE, VIDEO, SOUND,

  // graphical tags
  GRAPHICS,
  SUPERPOSE,
  GR_GROUP, GR_LINEAR_TRANSFORM,
  TEXT_AT, _POINT,
  LINE, CLINE, ARC, CARC, SPLINE, VAR_SPLINE, CSPLINE,
  FILL, IMAGE,
  BOX_INFO, FRAME_DIRECT, FRAME_INVERSE,

  // graphical interface markup
  CANVAS, ORNAMENT,

  // obsolete tags
  FORMAT, LINE_SEP, SPLIT, DELAY, HOLD, RELEASE,
  OLD_MATRIX, OLD_TABLE, OLD_MOSAIC, OLD_MOSAIC_ITEM,
  SET, RESET, EXPAND, VAR_EXPAND, HIDE_EXPAND,
  APPLY, BEGIN, END, FUNC, ENV,
  AUTHORIZE, POSTSCRIPT,

  // user extensions
  START_EXTENSIONS
};

inline tree_label SUB (bool right) { return right? RSUB: LSUB; }
inline tree_label SUP (bool right) { return right? RSUP: LSUP; }

/******************************************************************************
* Conversions from tree_labels to strings and vice versa
******************************************************************************/

void make_tree_label (tree_label l, string s);
tree_label make_tree_label (string s); // for extensions
string as_string (tree_label l);
tree_label as_tree_label (string s);
bool existing_tree_label (string s);

#ifdef OS_WIN32
inline bool operator == (tree_label first, tree_label second) {
  return (int) first == (int) second; }
inline bool operator != (tree_label first, tree_label second) {
  return (int) first != (int) second; }
#endif

#endif // defined TREE_LABEL_H
