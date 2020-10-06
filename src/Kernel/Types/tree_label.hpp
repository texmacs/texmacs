
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
  RIGID, HGROUP, HIDDEN, FREEZE, UNFREEZE,
  HSPACE, VAR_VSPACE, VSPACE, SPACE, HTAB,
  MOVE, SHIFT, RESIZE, CLIPPED,
  REPEAT, VAR_REPEAT, _FLOAT, DATOMS, DLINES, DPAGES, DBOX,
  LINE_NOTE, PAGE_NOTE, IF_PAGE_BREAK,

  // zero-ary formatting directives
  // modify is_formatting predicate when inserting new tags
  WITH_LIMITS, LINE_BREAK, NEW_LINE, NEXT_LINE,
  NO_BREAK, YES_INDENT, NO_INDENT, VAR_YES_INDENT, VAR_NO_INDENT,
  VAR_PAGE_BREAK, PAGE_BREAK, VAR_NO_PAGE_BREAK, NO_PAGE_BREAK,
  VAR_NO_BREAK_HERE, NO_BREAK_HERE, NO_BREAK_START, NO_BREAK_END,
  VAR_NEW_PAGE, NEW_PAGE, VAR_NEW_DPAGE, NEW_DPAGE,

  // mathematics
  AROUND, VAR_AROUND, BIG_AROUND,
  LEFT, MID, RIGHT, BIG, LONG_ARROW,
  LPRIME, RPRIME, BELOW, ABOVE,
  LSUB, LSUP, RSUB, RSUP,
  FRAC, SQRT, WIDE, VAR_WIDE, NEG, TREE,
  SYNTAX,

  // tabular material
  TFORMAT, TWITH, CWITH, TMARKER,
  TABLE, ROW, CELL, SUBTABLE,

  // macro language
  ASSIGN, PROVIDE, WITH, PROVIDES, VALUE, QUOTE_VALUE, OR_VALUE,
  MACRO, DRD_PROPS, ARG, QUOTE_ARG, COMPOUND,
  XMACRO, GET_LABEL, GET_ARITY, MAP_ARGS, EVAL_ARGS,
  NEW_THEME, COPY_THEME, APPLY_THEME, SELECT_THEME,
  MARK, EXPAND_AS,
  EVAL, QUOTE, QUASI, QUASIQUOTE, UNQUOTE, VAR_UNQUOTE, COPY,
  IF, VAR_IF, CASE, WHILE, FOR_EACH,
  EXTERN, VAR_INCLUDE, INCLUDE,
  WITH_PACKAGE, USE_PACKAGE, USE_MODULE,

  // computational markup
  OR, XOR, AND, NOT,
  PLUS, MINUS, TIMES, OVER, DIV, MOD, MINIMUM, MAXIMUM,
  MATH_SQRT, EXP, LOG, POW, COS, SIN, TAN,
  MERGE, LENGTH, RANGE, NUMBER, _DATE, TRANSLATE, CHANGE_CASE,
  FIND_FILE, FIND_FILE_UPWARDS,
  IS_TUPLE, LOOK_UP, OCCURS_INSIDE,
  EQUAL, UNEQUAL, LESS, LESSEQ, GREATER, GREATEREQ,
  BLEND, RGB_COLOR, RGB_ACCESS,

  // built-in length units
  CM_LENGTH, MM_LENGTH, IN_LENGTH, PT_LENGTH,
  BP_LENGTH, DD_LENGTH, PC_LENGTH, CC_LENGTH,     // standard absolute units
  FS_LENGTH, FBS_LENGTH, EM_LENGTH,
  LN_LENGTH, SEP_LENGTH, YFRAC_LENGTH, EX_LENGTH, // font specific
  FN_LENGTH, FNS_LENGTH, BLS_LENGTH,
  FNBOT_LENGTH, FNTOP_LENGTH,
  SPC_LENGTH, XSPC_LENGTH,                        // font specific & stretchy
  PAR_LENGTH, PAG_LENGTH,                         // other
  GW_LENGTH, GH_LENGTH, GU_LENGTH,                // extents & unit of graphics
  TMPT_LENGTH, PX_LENGTH,                         // miscellaneous
  LCORNER_LENGTH, BCORNER_LENGTH,
  RCORNER_LENGTH, TCORNER_LENGTH,
  MS_LENGTH, S_LENGTH, MSEC_LENGTH, SEC_LENGTH,   // durations
  MIN_LENGTH, HR_LENGTH,

  // tags for source tree editing
  FILTER_STYLE,
  STYLE_WITH, VAR_STYLE_WITH, STYLE_ONLY, VAR_STYLE_ONLY,
  ACTIVE, VAR_ACTIVE, INACTIVE, VAR_INACTIVE,
  REWRITE_INACTIVE, INLINE_TAG, OPEN_TAG, MIDDLE_TAG, CLOSE_TAG,
  SYMBOL, LATEX, HYBRID,

  // linking primitives
  LOCUS, ID, HARD_ID,
  LINK, URL, SCRIPT, OBSERVER,
  FIND_ACCESSIBLE, HLINK, ACTION,
  SET_BINDING, GET_BINDING, HAS_BINDING, HIDDEN_BINDING,
  LABEL, REFERENCE, PAGEREF,
  GET_ATTACHMENT, WRITE, TOC_NOTIFY,

  // various other tags
  TUPLE, ATTR, TMLEN, COLLECTION, ASSOCIATE, BACKUP, PATTERN, GRADIENT,
  SPECIFIC, FLAG, HYPHENATE_AS,

  // animations
  ANIM_STATIC, ANIM_DYNAMIC, MORPH,
  ANIM_TIME, ANIM_PORTION,
  ANIM_COMPOSE, ANIM_REPEAT, ANIM_CONSTANT, ANIM_ACCELERATE,
  ANIM_TRANSLATE, ANIM_PROGRESSIVE, VIDEO, SOUND,

  // graphical tags
  GRAPHICS,
  SUPERPOSE,
  GR_GROUP, GR_TRANSFORM, GR_EFFECT,
  TEXT_AT, MATH_AT, DOCUMENT_AT, _POINT,
  LINE, CLINE, ARC, CARC,
  SPLINE, VAR_SPLINE, CSPLINE,
  BEZIER, CBEZIER, SMOOTH, CSMOOTH,
  FILL, IMAGE,
  BOX_INFO, FRAME_DIRECT, FRAME_INVERSE,
  IS_EQUAL, IS_INTERSECTION,
  ON_CURVE, ON_TEXT_BORDER, ON_GRID,
  TRANSFORM_3D, OBJECT_3D, TRIANGLE_3D,
  LIGHT_3D, LIGHT_DIFFUSE, LIGHT_SPECULAR,

  // graphical effects
  EFF_MOVE, EFF_MAGNIFY, EFF_BUBBLE, EFF_CROP,
  EFF_TURBULENCE, EFF_FRACTAL_NOISE,
  EFF_HATCH, EFF_DOTS,
  EFF_GAUSSIAN, EFF_OVAL, EFF_RECTANGULAR, EFF_MOTION,
  EFF_BLUR, EFF_OUTLINE, EFF_THICKEN, EFF_ERODE,
  EFF_DEGRADE, EFF_DISTORT, EFF_GNAW,
  EFF_SUPERPOSE, EFF_ADD, EFF_SUB, EFF_MUL, EFF_MIN, EFF_MAX, EFF_MIX,
  EFF_NORMALIZE, EFF_MONOCHROME, EFF_COLOR_MATRIX, EFF_GRADIENT,
  EFF_MAKE_TRANSPARENT, EFF_MAKE_OPAQUE, EFF_RECOLOR, EFF_SKIN,

  // graphical interface markup
  CANVAS, ORNAMENT, ART_BOX,

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


#endif // defined TREE_LABEL_H
