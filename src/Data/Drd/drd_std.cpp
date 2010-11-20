
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

#define regular(i) type (i, TYPE_REGULAR)
#define raw(i) type (i, TYPE_RAW)
#define macro(i) type (i, TYPE_MACRO)
#define argument(i) type (i, TYPE_ARGUMENT)
#define variable(i) type (i, TYPE_VARIABLE)
#define binding(i) type (i, TYPE_BINDING)
#define boolean(i) type (i, TYPE_BOOLEAN)
#define integer(i) type (i, TYPE_INTEGER)
#define string_type(i) type (i, TYPE_STRING)
#define numeric(i) type (i, TYPE_NUMERIC)
#define length(i) type (i, TYPE_LENGTH)
#define code(i) type (i, TYPE_CODE)
#define url_type(i) type (i, TYPE_URL)
#define identifier(i) type (i, TYPE_IDENTIFIER)
#define graphical(i) type (i, TYPE_GRAPHICAL)
#define point_type(i) type (i, TYPE_POINT)
#define animation(i) type (i, TYPE_ANIMATION)
#define duration(i) type (i, TYPE_DURATION)

#define returns_adhoc() type (TYPE_ADHOC)
#define returns_boolean() type (TYPE_BOOLEAN)
#define returns_integer() type (TYPE_INTEGER)
#define returns_string() type (TYPE_STRING)
#define returns_numeric() type (TYPE_NUMERIC)
#define returns_length() type (TYPE_LENGTH)
#define returns_url() type (TYPE_URL)
#define returns_identifier() type (TYPE_IDENTIFIER)
#define returns_animation() type (TYPE_ANIMATION)
#define returns_graphical() type (TYPE_GRAPHICAL)

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
  std_drd->freeze_border (l);
  // std_drd->freeze_block (l);
  // FIXME: freeze children properties
}

static bool std_drd_initialized= false;

void
init_std_drd () {
  if (std_drd_initialized) return;
  std_drd_initialized=true;

  init (STRING, "string", fixed (0) -> returns_string ());
  init (UNKNOWN, "unknown", fixed (0));
  init (UNINIT, "uninit", fixed (0));
  init (ERROR, "error", fixed (1));
  init (RAW_DATA, "raw-data", fixed (1) -> raw (0));

  init (DOCUMENT, "document",
	repeat (1, 1) -> inner_border () -> accessible (0));
  init (PARA, "para",
	repeat (1, 1) -> inner_border () -> accessible (0) ->
	name ("paragraph"));
  init (SURROUND, "surround", fixed (3) -> accessible (0));
  init (CONCAT, "concat", repeat (1, 1) -> inner_border () -> accessible (0));
  init (RIGID, "rigid", fixed (1) -> accessible (0));
  init (HIDDEN, "hidden", fixed (1) -> inner_border () -> hidden (0));
  init (FREEZE, "freeze",
	fixed (1) -> inner_border () -> regular (0));
  init (UNFREEZE, "unfreeze", fixed (1) -> accessible (0) -> inner_border ());
  init (HSPACE, "hspace",
	options (1, 2) -> length (0) ->	name ("horizontal space"));
  init (VAR_VSPACE, "vspace*",
	options (1, 2) -> length (0) ->	name ("vertical space before"));
  init (VSPACE, "vspace",
	options (1, 2) -> length (0) ->	name ("vertical space"));
  init (SPACE, "space",
	options (1, 2) -> length (0));
  // space markup has arity 1 or 3
  init (HTAB, "htab",
	options (1, 1, BIFORM) -> length (0) -> name ("tab"));
  init (MOVE, "move",
        fixed (3, 0, DETAILED) ->
        accessible (0) ->
        length (1) -> name (1, "x-offset") ->
        length (2) -> name (2, "y-offset"));
  init (SHIFT, "shift",
        fixed (3, 0, DETAILED) ->
        accessible (0) ->
        length (1) -> name (1, "x-offset") ->
        length (2) -> name (2, "y-offset"));
  init (RESIZE, "resize",
        fixed (5, 0, DETAILED) ->
        accessible (0) ->
        length (1) -> name (1, "left") ->
        length (2) -> name (2, "bottom") ->
        length (3) -> name (3, "right") ->
        length (4) -> name (4, "top"));
  init (CLIPPED, "clipped",
        fixed (5, 0, DETAILED) ->
        accessible (0) ->
        length (1) -> name (1, "left") ->
        length (2) -> name (2, "bottom") ->
        length (3) -> name (3, "right") ->
        length (4) -> name (4, "top"));
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

  init (AROUND, "around", fixed (3, 0, DETAILED) ->
        name (0, "left bracket") ->
        accessible (1) ->
        name (2, "right bracket"));
  init (VAR_AROUND, "around*", fixed (3, 0, DETAILED) ->
        name (0, "left bracket") ->
        accessible (1) ->
        name (2, "right bracket"));
  init (BIG_AROUND, "big-around", fixed (2, 0, DETAILED) ->
        name (0, "big operator") ->
        accessible (1));
  init (LEFT, "left", options (1, 2));
  init (MID, "mid", options (1, 2));
  init (RIGHT, "right", options (1, 2));
  init (BIG, "big", fixed (1));
  init (LPRIME, "lprime", fixed (1) -> name ("left prime"));
  init (RPRIME, "rprime", fixed (1) -> name ("right prime"));
  init (BELOW, "below", fixed (2) -> accessible (0));
  init (ABOVE, "above", fixed (2) -> accessible (0));
  init (LSUB, "lsub",
	fixed (1) -> name ("left subscript") ->
        accessible (0) -> locals (0, "math-display", "false"));
  init (LSUP, "lsup",
	fixed (1) -> name ("left superscript") ->
        accessible (0) -> locals (0, "math-display", "false"));
  init (RSUB, "rsub",
	fixed (1) -> name ("subscript") ->
        accessible (0) -> locals (0, "math-display", "false"));
  init (RSUP, "rsup",
	fixed (1) -> name ("superscript") ->
        accessible (0) -> locals (0, "math-display", "false"));
  init (FRAC, "frac",
        fixed (2) -> name ("fraction") ->
        accessible (0) -> locals (0, "math-display", "false"));
  init (SQRT, "sqrt", options (1, 1) -> accessible (0) -> name ("root"));
  init (WIDE, "wide", fixed (1, 1, BIFORM) ->
        accessible (0) ->
        name (1, "accent"));
  init (VAR_WIDE, "wide*", fixed (1, 1, BIFORM) ->
        accessible (0) -> name ("wide under") ->
        name (1, "accent"));
  init (NEG, "neg", fixed (1) -> accessible (0) -> name ("negation"));
  init (TREE, "tree", repeat (2, 1) -> accessible (0));

  init (TFORMAT, "tformat",
	var_repeat (1, 1, BIFORM) -> inner_border () ->
	accessible (1) ->
	name ("table format"));
  init (TWITH, "twith",
	fixed (2) -> returns_adhoc () ->
	accessible (0) -> binding (0) ->
	name ("table property"));
  init (CWITH, "cwith",
	fixed (4, 2, BIFORM) -> returns_adhoc () ->
	accessible (0) -> integer (0) ->
	accessible (1) -> binding (1) ->
	name ("cell property"));
  init (TMARKER, "tmarker", fixed (0) -> name ("table marker"));
  init (TABLE, "table", repeat (1, 1) -> inner_border () -> accessible (0));
  init (ROW, "row", repeat (1, 1) -> inner_border () -> accessible (0));
  init (CELL, "cell", fixed (1) -> inner_border () -> accessible (0));
  init (SUBTABLE, "subtable", fixed (1) -> inner_border () -> accessible (0));

  init (ASSIGN, "assign",
	fixed (1, 1, BIFORM) -> variable (0) -> regular (1));
  init (WITH, "with",
	var_repeat (2, 1, BIFORM) -> with_like () ->
	binding (0) -> accessible (1));
  init (PROVIDES, "provides",
	fixed (1) -> returns_boolean () -> string_type (0));
  init (VALUE, "value", fixed (1) -> variable (0));
  init (QUOTE_VALUE, "quote-value",
	fixed (1) -> variable (0) -> name ("quoted value"));
  init (MACRO, "macro",
	var_repeat (1, 1, BIFORM) -> argument (0) -> regular (1));
  init (DRD_PROPS, "drd-props",
	repeat (3, 2) -> name ("drd properties"));
  init (QUOTE_ARG, "quote-arg",
	repeat (1, 1, BIFORM) -> argument (0) -> name ("quoted argument"));
  init (ARG, "arg",
	repeat (1, 1, BIFORM) -> argument (0) -> name ("argument"));
  init (COMPOUND, "compound",
	repeat (1, 1, BIFORM) -> variable (0) -> accessible (1));
  // FIXME: should be refined. The current setting is f.i. needed for "theorem"
  init (XMACRO, "xmacro",
	fixed (1, 1, BIFORM) -> argument (0) -> regular (1));
  init (GET_LABEL, "get-label", fixed (1) -> returns_string ());
  init (GET_ARITY, "get-arity", fixed (1) -> returns_integer ());
  init (MAP_ARGS, "map-args",
	options (3, 2, DETAILED) ->
	variable (0) -> name (0, "macro") ->
	variable (1) -> name (1, "return tag") ->
	argument (2) -> name (2, "argument") ->
	integer (3) -> name (3, "start index") ->
	integer (4) -> name (4, "end index") ->
	name ("map arguments"));
  init (EVAL_ARGS, "eval-args",
	fixed (1) -> argument (0) -> name ("evaluate arguments"));
  init (MARK, "mark", fixed (2));
  init (EXPAND_AS, "expand-as", fixed (2));
  init (EVAL, "eval", fixed (1) -> name ("evaluate"));
  init (QUOTE, "quote", fixed (1) -> regular (0));
  init (QUASI, "quasi", fixed (1) -> regular (0));
  init (QUASIQUOTE, "quasiquote", fixed (1) -> regular (0));
  init (UNQUOTE, "unquote", fixed (1) -> regular (0));
  init (VAR_UNQUOTE, "unquote*", fixed (1) -> regular (0));
  init (COPY, "copy", fixed (1) -> regular (0));
  init (IF, "if",
	options (2, 1, DETAILED) ->
	boolean (0) -> name (0, "condition") ->
        regular (1) -> regular (2));
  init (VAR_IF, "if*",
	fixed (1, 1, BIFORM) ->
	boolean (0) -> name (0, "condition") ->
        regular (1));
  init (CASE, "case",
	repeat (2, 1));
  init (WHILE, "while",
	fixed (1, 1, BIFORM) -> boolean (0) -> regular (1));
  init (FOR_EACH, "for-each",
	fixed (1, 1, BIFORM) -> variable (0) -> regular (1));
  init (EXTERN, "extern",
	repeat (1, 1, BIFORM) -> code (0) -> regular (1)); // func and args
  init (INCLUDE, "include", fixed (1) -> url_type (0));
  init (USE_PACKAGE, "use-package", repeat (1, 1) -> string_type (0));
  init (USE_MODULE, "use-module", repeat (1, 1) -> code (0));

  init (OR, "or", repeat (2, 1) -> returns_boolean () -> boolean (0));
  init (XOR, "xor", fixed (2) -> returns_boolean () -> boolean (0));
  init (AND, "and", repeat (2, 1) -> returns_boolean () -> boolean (0));
  init (NOT, "not", fixed (1) -> returns_boolean () -> boolean (0));
  init (PLUS, "plus", repeat (2, 1) -> returns_numeric () -> numeric (0));
  init (MINUS, "minus", repeat (1, 1) -> returns_numeric () -> numeric (0));
  init (TIMES, "times", repeat (2, 1) -> returns_numeric () -> numeric (0));
  init (OVER, "over", repeat (1, 1) -> returns_numeric () -> numeric (0));
  init (DIV, "div",
	fixed (2) -> returns_numeric () -> numeric (0) -> name ("divide"));
  init (MOD, "mod",
	fixed (2) -> returns_numeric () -> numeric (0) -> name ("modulo"));
  init (MINIMUM, "minimum",
	repeat (2, 1) -> returns_numeric () -> numeric (0));
  init (MAXIMUM, "maximum",
	repeat (1, 1) -> returns_numeric () -> numeric (0));
  init (MATH_SQRT, "math-sqrt",
	fixed (1) -> returns_numeric () -> numeric (0) ->
	name ("square root"));
  init (EXP, "exp", fixed (1) -> returns_numeric () -> numeric (0));
  init (LOG, "log", fixed (1) -> returns_numeric () -> numeric (0));
  init (POW, "pow", fixed (2) -> returns_numeric () -> numeric (0));
  init (COS, "cos", fixed (1) -> returns_numeric () -> numeric (0));
  init (SIN, "sin", fixed (1) -> returns_numeric () -> numeric (0));
  init (TAN, "tan", fixed (1) -> returns_numeric () -> numeric (0));
  init (MERGE, "merge", repeat (2, 1) -> returns_adhoc ());
  init (LENGTH, "length", fixed (1) -> returns_integer ());
  init (RANGE, "range",
	fixed (1, 2, BIFORM) -> returns_adhoc () -> accessible (0));
  init (NUMBER, "number",
	fixed (2) -> returns_string () -> string_type (0));
  init (_DATE, "date",
	options (0, 2) -> returns_string () -> string_type (0));
  init (TRANSLATE, "translate",
	fixed (3) -> returns_string () -> string_type (0));
  init (CHANGE_CASE, "change-case",
	fixed (1, 1, BIFORM) -> accessible (0) -> string_type (1));
  init (FIND_FILE, "find-file",
	var_repeat (1, 1) -> returns_url () -> url_type (0)); // dirs and file
  init (IS_TUPLE, "is-tuple",
	fixed (1) -> returns_boolean () -> regular (0) -> name ("tuple?"));
  init (LOOK_UP, "look-up",
	fixed (1, 1, BIFORM) -> regular (0) -> integer (1));
  init (EQUAL, "equal",
	fixed (2) -> returns_boolean () -> regular (0));
  init (UNEQUAL, "unequal",
	fixed (2) -> returns_boolean () -> regular (0) ->
	name ("not equal"));
  init (LESS, "less",
	fixed (2) -> returns_boolean () -> regular (0));
  init (LESSEQ, "lesseq",
	fixed (2) -> returns_boolean () -> regular (0) ->
	name ("less or equal"));
  init (GREATER, "greater",
	fixed (2) -> returns_boolean () -> regular (0));
  init (GREATEREQ, "greatereq",
	fixed (2) -> returns_boolean () -> regular (0) ->
	name ("greater or equal"));

  init (CM_LENGTH, "cm-length", fixed (0) -> returns_length ());
  init (MM_LENGTH, "mm-length", fixed (0) -> returns_length ());
  init (IN_LENGTH, "in-length", fixed (0) -> returns_length ());
  init (PT_LENGTH, "pt-length", fixed (0) -> returns_length ());
  init (BP_LENGTH, "bp-length", fixed (0) -> returns_length ());
  init (DD_LENGTH, "dd-length", fixed (0) -> returns_length ());
  init (PC_LENGTH, "pc-length", fixed (0) -> returns_length ());
  init (CC_LENGTH, "cc-length", fixed (0) -> returns_length ());
  init (FS_LENGTH, "fs-length", fixed (0) -> returns_length ());
  init (FBS_LENGTH, "fbs-length", fixed (0) -> returns_length ());
  init (EM_LENGTH, "em-length", fixed (0) -> returns_length ());
  init (LN_LENGTH, "ln-length", fixed (0) -> returns_length ());
  init (SEP_LENGTH, "sep-length", fixed (0) -> returns_length ());
  init (YFRAC_LENGTH, "yfrac-length", fixed (0) -> returns_length ());
  init (EX_LENGTH, "ex-length", fixed (0) -> returns_length ());
  init (FN_LENGTH, "fn-length", fixed (0) -> returns_length ());
  init (FNS_LENGTH, "fns-length", fixed (0) -> returns_length ());
  init (BLS_LENGTH, "bls-length", fixed (0) -> returns_length ());
  init (FNBOT_LENGTH, "fnbot-length", fixed (0) -> returns_length ());
  init (FNTOP_LENGTH, "fntop-length", fixed (0) -> returns_length ());
  init (SPC_LENGTH, "spc-length", fixed (0) -> returns_length ());
  init (XSPC_LENGTH, "xspc-length", fixed (0) -> returns_length ());
  init (PAR_LENGTH, "par-length", fixed (0) -> returns_length ());
  init (PAG_LENGTH, "pag-length", fixed (0) -> returns_length ());
  init (GW_LENGTH, "gw-length", fixed (0) -> returns_length ());
  init (GH_LENGTH, "gh-length", fixed (0) -> returns_length ());
  init (TMPT_LENGTH, "tmpt-length", fixed (0) -> returns_length ());
  init (PX_LENGTH, "px-length", fixed (0) -> returns_length ());
  init (MSEC_LENGTH, "msec-length", fixed (0) -> returns_length ());
  init (SEC_LENGTH, "sec-length", fixed (0) -> returns_length ());
  init (MIN_LENGTH, "min-length", fixed (0) -> returns_length ());
  init (HR_LENGTH, "hr-length", fixed (0) -> returns_length ());

  init (STYLE_WITH, "style-with",
	var_repeat (2, 1, BIFORM) -> binding (0) -> accessible (1));
  init (VAR_STYLE_WITH, "style-with*",
	var_repeat (2, 1, BIFORM) -> binding (0) -> accessible (1));
  init (STYLE_ONLY, "style-only", fixed (1) -> accessible (0));
  init (VAR_STYLE_ONLY, "style-only*", fixed (1) -> accessible (0));
  init (ACTIVE, "active", fixed (1) -> accessible (0));
  init (VAR_ACTIVE, "active*", fixed (1) -> accessible (0));
  init (INACTIVE, "inactive", fixed (1) -> accessible (0));
  init (VAR_INACTIVE, "inactive*", fixed (1) -> accessible (0));
  init (REWRITE_INACTIVE, "rewrite-inactive", fixed (2));
  init (INLINE_TAG, "inline-tag",
	repeat (1, 1, BIFORM) ->
	accessible (0) -> variable (0) -> name (0, "macro") ->
        accessible (1));
  init (OPEN_TAG, "open-tag",
	repeat (1, 1, BIFORM) ->
	accessible (0) -> variable (0) -> name (0, "macro") ->
        accessible (1));
  init (MIDDLE_TAG, "middle-tag",
	repeat (1, 1, BIFORM) ->
	variable (0) -> name (0, "macro") ->
        accessible (1));
  init (CLOSE_TAG, "close-tag",
	repeat (1, 1, BIFORM) ->
	variable (0) -> name (0, "macro") ->
        accessible (1));
  init (SYMBOL, "symbol",
        fixed (1) ->
        code (0) -> name (0, "name") -> locals (0, "mode", "src"));
  init (LATEX, "latex",
	fixed (1) ->
        code (0) -> name (0, "command") -> locals (0, "mode", "src"));
  init (HYBRID, "hybrid",
	options (1, 1, BIFORM) ->
	variable (0) -> name (0, "command") -> locals (0, "mode", "src") ->
	accessible (1));

  init (LOCUS, "locus",
	var_repeat (1, 1, BIFORM) ->
	accessible (1));
  init (ID, "id",
	repeat (1, 1) -> returns_adhoc () ->
	accessible (0) -> identifier (0));
  init (HARD_ID, "hard-id",
	options (0, 1) -> returns_identifier () ->
	regular (0));
  init (LINK, "link",
	repeat (1, 1, BIFORM) -> returns_adhoc () ->
	accessible (0) -> string_type (0) -> name (0, "link type") ->
	accessible (1) -> name (1, "participants"));
  init (URL, "url",
	options (1, 1, BIFORM) -> returns_adhoc () ->
	accessible (0) -> url_type (0) ->
	accessible (1) -> regular (1));       // FIXME: location?
  init (SCRIPT, "script",
	options (1, 1, BIFORM) -> returns_adhoc () ->
	accessible (0) -> code (0) -> name (0, "script") ->
	accessible (1) -> regular (1) -> name (0, "where"));
  init (HLINK, "hlink",
	fixed (1, 1, BIFORM) ->
	accessible (0) -> name (0, "text") ->
	url_type (1) -> name (1, "destination") ->
	name ("hyperlink"));
  init (ACTION, "action",
	options (2, 1, DETAILED) ->
	accessible (0) -> name (0, "text") ->
	code (1) -> name (1, "script") ->
	regular (2) -> name (2, "where"));
  init (SET_BINDING, "set-binding",
	options (1, 2));                      // see env_exec.cpp
  init (GET_BINDING, "get-binding",
	options (1, 1, BIFORM) ->
	identifier (0) -> name (0, "key") ->
	integer (1) -> name (0, "kind"));
  init (LABEL, "label",
	fixed (1) ->
        identifier (0) -> name (0, "id") -> long_name (0, "identifier"));
  init (REFERENCE, "reference",
	fixed (1) ->
        identifier (0) -> name (0, "id") -> long_name (0, "identifier"));
  init (PAGEREF, "pageref",
	fixed (1) -> name ("page reference") ->
        identifier (0) -> name (0, "id") -> long_name (0, "identifier"));
  init (WRITE, "write",
	fixed (1, 1, BIFORM) ->
	string_type (0) -> name (0, "channel") ->
	regular (1) -> name (1, "content"));

  init (TUPLE, "tuple",
	repeat (0, 1) -> accessible (0));
  init (ATTR, "attr",
	repeat (2, 2) -> accessible (0) -> binding (0) ->
	name ("attributes"));
  init (TMLEN, "tmlen",
	options (1, 2) -> returns_length () ->
	length (0) -> name ("TeXmacs length"));
  init (COLLECTION, "collection",
	repeat (1, 1) -> regular (0));
  init (ASSOCIATE, "associate",
	fixed (2) -> binding (0));
  init (BACKUP, "backup",
	fixed (2) -> regular (0));
  init (PATTERN, "pattern",
	options (3, 1, DETAILED) -> url_type (0));
  init (GRADIENT, "gradient",
	fixed (3));                           // not yet implemented
  init (SPECIFIC, "specific",
	fixed (1, 1, BIFORM) ->
	string_type (0) -> name (0, "medium") ->
	regular (1) -> name (1, "content"));
  init (FLAG, "flag",
	options (2, 1, DETAILED) ->
	regular (0) -> name (0, "flag text") ->
	string_type (1) -> name (1, "color") ->
	argument (2) -> name (2, "source"));

  init (ANIM_COMPOSE, "anim-compose",
	repeat (1, 1) -> returns_animation () ->
	animation (0));
  init (ANIM_REPEAT, "anim-repeat",
	fixed (1) -> returns_animation () ->
	accessible (0) -> animation (0));
  init (ANIM_CONSTANT, "anim-constant",
	fixed (1, 1, BIFORM) -> returns_animation () ->
	accessible (0) -> duration (1));
  init (ANIM_TRANSLATE, "anim-translate",
	fixed (1, 3, DETAILED) -> returns_animation () ->
	accessible (0) -> animation (0) ->
	duration (1));
  init (ANIM_PROGRESSIVE, "anim-progressive",
	fixed (1, 3, DETAILED) -> returns_animation () ->
	accessible (0) -> animation (0) ->
	duration (1));
  init (VIDEO, "video",
	fixed (1, 4, BIFORM) -> url_type (0));
  init (SOUND, "sound",
	fixed (1) -> url_type (0));

  init (GRAPHICS, "graphics",
	repeat (1, 1) -> accessible (0) -> graphical (0));
  init (SUPERPOSE, "superpose",
	repeat (1, 1) -> accessible (0));
  init (GR_GROUP, "gr-group",
	repeat (1, 1) -> returns_graphical () -> graphical (0));
  init (GR_LINEAR_TRANSFORM, "gr-linear-transform",
	fixed (1, 1, BIFORM) -> returns_graphical () ->
	graphical (0));
  init (TEXT_AT, "text-at",
	fixed (1, 1, BIFORM) -> returns_graphical () ->
	accessible (0) -> point_type (1));
  init (_POINT, "point",
	repeat (1, 1) -> returns_graphical () -> point_type (0));
  init (LINE, "line",
	repeat (2, 1) -> returns_graphical () -> point_type (0));
  init (CLINE, "cline",
	repeat (3, 1) -> returns_graphical () -> point_type (0));
  init (ARC, "arc",
	repeat (3, 1) -> returns_graphical () -> point_type (0));
  init (CARC, "carc",
	repeat (3, 1) -> returns_graphical () -> point_type (0));
  init (SPLINE, "spline",
	repeat (2, 1) -> returns_graphical () -> point_type (0));
  init (VAR_SPLINE, "spline*",
	repeat (2, 1) -> returns_graphical () -> point_type (0));
  init (CSPLINE, "cspline",
	repeat (2, 1) -> returns_graphical () -> point_type (0));
  init (FILL, "fill",
	repeat (1, 1));                       // Not yet implemented
  init (IMAGE, "image",
	fixed (1, 4, DETAILED) ->
	url_type (0) -> name (0, "url") ->
	length (1) -> name (1, "width") ->
	length (2) -> name (2, "height") ->
        length (3) -> name (3, "x") -> long_name (3, "x-offset") ->
        length (4) -> name (4, "y") -> long_name (4, "y-offset"));

  init (BOX_INFO, "box-info",
	fixed (1, 1, BIFORM) ->
	regular (0) ->                        // content leading to box
	string_type (1));                     // query
  init (FRAME_DIRECT, "frame-direct",
	fixed (1) -> point_type (0));
  init (FRAME_INVERSE, "frame-inverse",
	fixed (1) -> point_type (0));

  init (CANVAS, "canvas", fixed (6, 1, BIFORM) -> accessible (1));
  init (ORNAMENT, "ornament", fixed (1) -> accessible (0));

  init (FORMAT, "format",
	repeat (1, 1));
  init (LINE_SEP, "line-sep",
	fixed (0) -> name ("line separator"));
  init (SPLIT, "split",
	repeat (1, 1));
  init (DELAY, "delay",
	fixed (1) -> regular (0));
  init (HOLD, "hold",
	fixed (1) -> regular (0));
  init (RELEASE, "release",
	fixed (1) -> regular (0));
  init (OLD_MATRIX, "old-matrix",
	var_repeat (1, 2, BIFORM) -> accessible (0));
  init (OLD_TABLE, "old-table",
	var_repeat (1, 2, BIFORM) -> accessible (0));
  init (OLD_MOSAIC, "old-mosaic",
	var_repeat (1, 2, BIFORM) -> accessible (0));
  init (OLD_MOSAIC_ITEM, "old-mosaic-item",
	repeat (1, 1) -> accessible (0));
  init (SET, "set",
	fixed (1, 1, BIFORM) -> variable (0) -> regular (0));
  init (RESET, "reset",
	fixed (1) -> variable (0));
  init (EXPAND, "expand",
	repeat (1, 1, BIFORM) ->
	variable (0) -> accessible (1));
  init (VAR_EXPAND, "expand*",
	repeat (1, 1, BIFORM) -> inner_border () ->
	variable (0) -> accessible (1));
  init (HIDE_EXPAND, "hide-expand",
	repeat (2, 1, DETAILED) ->
	accessible (1));
  init (APPLY, "apply",
	repeat (1, 1, BIFORM) -> variable (0));
  init (BEGIN, "begin",
	repeat (1, 1, BIFORM) -> variable (0));
  init (END, "end",
	fixed (1) -> variable (0));
  init (FUNC, "func",
	var_repeat (1, 1, BIFORM) ->
	argument (0) -> regular (1));
  init (ENV, "env",
	var_repeat (1, 2, BIFORM) ->
	argument (0));
  init (AUTHORIZE, "authorize",
	fixed (2));
  init (POSTSCRIPT, "postscript",
	fixed (1, 6, BIFORM) -> url_type (0));

  init (make_tree_label ("shown"), "shown",
	fixed (1) -> accessible (0) -> inner_border ());
  init (make_tree_label ("ignore"), "ignore",
	fixed (1) -> regular (0) -> inner_border ());
}
