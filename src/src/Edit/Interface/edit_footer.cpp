
/******************************************************************************
* MODULE     : edit_footer.cpp
* DESCRIPTION: display interesting information for the user in the footer
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "edit_interface.hpp"
#include "convert.hpp"

/******************************************************************************
* Set left footer with information about environment variables
******************************************************************************/

void
edit_interface_rep::set_left_footer (string s) {
  SERVER (set_left_footer (s));
}

void
edit_interface_rep::append_left_footer (string& s, string env_var) {
  string i= get_init_string (env_var);
  string c= get_env_string (env_var);
  if (c != i) s= s * "#" * c;
}

void
edit_interface_rep::set_left_footer () {
  int i;
  string s, r, e;
  double base_sz= get_env_int (FONT_BASE_SIZE);
  double sz= get_env_double (FONT_SIZE);
  tree the_style= get_style ();
  for (i=0; i<arity (the_style); i++)
    s= s * "#" * as_string (the_style[i]);
  string mode= get_env_string (MODE);
  string lan = get_env_string (LANGUAGE (mode));
  if (mode == "prog") s= s * "#program";
  else if (as_string (get_init_value (LANGUAGE (mode))) != lan)
    s= s * "#" * lan;
  else s= s * "#" * mode;
  if (mode == "text") {
    s= s * "#" * get_env_string (TEXT_FONT);
    append_left_footer (s, TEXT_FAMILY);
    s= s * "#" * as_string ((int) ((base_sz+0.5)*sz));
    append_left_footer (s, TEXT_SERIES);
    append_left_footer (s, TEXT_SHAPE);
  }
  else if (mode == "math") {
    s= s * "#" * get_env_string (MATH_FONT);
    append_left_footer (s, MATH_FAMILY);
    s= s * "#" * as_string ((int) ((base_sz+0.5)*sz));
    append_left_footer (s, MATH_SERIES);
    append_left_footer (s, MATH_SHAPE);
  }
  else {
    string session_name= get_env_string (THIS_SESSION);
    if (session_name != "default") s= s * "-" * session_name;
    s= s * "#" * get_env_string (PROG_FONT);
    append_left_footer (s, PROG_FAMILY);
    s= s * "#" * as_string ((int) ((base_sz+0.5)*sz));
    append_left_footer (s, PROG_SERIES);
    append_left_footer (s, PROG_SHAPE);
  }
  r= get_env_string (COLOR);
  if (r != "black") s= s * "#" * r;
  if ((N(s)>0) && (s[0] == '#')) s= s (1, N(s));
  set_left_footer (s);
}

/******************************************************************************
* Set right footer with information about cursor position
******************************************************************************/

void
edit_interface_rep::set_right_footer (string s) {
  SERVER (set_right_footer (s));
}

string
edit_interface_rep::compute_text_footer (tree st) {
  string r;
  language lan= get_env_language ();
  int end  = last_item (tp);
  int start= end;
  if (lan->enc->token_backward (st->label, start))
    fatal_error ("bad cursor position in string",
		 "edit_interface_rep::set_footer");
  r= st->label (start, end);
  if (r == "") r= "start";
  if (r == " ") r= "space";
  if (r == "#") r= "sharp";
  return r;
}

static string
get_accent_type (string s) {
  if (s == "^") return "hat";
  if (s == "~") return "tilde";
  if ((N(s)>=2) && (s[0]=='<') && (s[N(s)-1]=='>')) return s (1, N(s)-1);
  return "unknown accent";
}

inline string
as_symbol (tree t) {
  string s= as_string (t);
  if (N(s)<=1) return s;
  else return "<" * s * ">";
}

static string
get_with_text (tree t) {
  int i, n=N(t), k=(n-1)/2;
  if ((n&1)!=1) return "";
  string s;
  for (i=0; i<k; i++)
    if (is_atomic (t[2*i]) && (t[2*i]!="") && is_atomic (t[2*i+1])) {
      if (i>0) s << "#";
      string var= t[2*i]->label;
      if ((var!=MODE) && (var!=COLOR) && (var!=PAR_MODE) &&
	  (var!=TEXT_LANGUAGE) && (var!=TEXT_FONT) &&
	  (var!=TEXT_FAMILY) && (var!=TEXT_SHAPE) && (var!=TEXT_SERIES) &&
	  (var!=MATH_LANGUAGE) && (var!=MATH_FONT) &&
	  (var!=MATH_FAMILY) && (var!=MATH_SHAPE) && (var!=MATH_SERIES) &&
	  (var!=PROG_LANGUAGE) && (var!=PROG_FONT) &&
	  (var!=PROG_FAMILY) && (var!=PROG_SHAPE) && (var!=PROG_SERIES) &&
	  (var!=THIS_SESSION))
	s << var << "=";
      s << t[2*i+1]->label;
    }
  return s;
}

string
edit_interface_rep::compute_operation_footer (tree st) {
  string r;
  switch (L (st)) {
  case RAW_DATA: r= "raw data"; break;
  case SURROUND: r= "surround"; break;
  case FORMAT: r= as_string (st[0]); break;
  case HSPACE: r= "horizontal space"; break;
  case VSPACE_BEFORE: r= "vertical space before"; break;
  case VSPACE_AFTER: r= "vertical space"; break;
  case SPACE: r= "rigid space"; break;
  case HTAB: r= "tab"; break;
  case SPLIT: r= "split"; break;
  case MOVE: r= "move"; break;
  case RESIZE: r= "resize"; break;
  case _FLOAT: r= (is_atomic (st[0])? st[0]->label: string ("float")); break;
  case REPEAT: r= "repeat"; break;
  case DECORATE_ATOMS: r= "decorate atoms"; break;
  case DECORATE_LINES: r= "decorate lines"; break;
  case DECORATE_PAGES: r= "decorate pages"; break;
  case WITH_LIMITS:
  case LINE_BREAK:
  case NEW_LINE:
  case LINE_SEP:
  case NEXT_LINE:
  case NO_BREAK:
  case NO_FIRST_INDENT:
  case YES_FIRST_INDENT:
  case NO_FIRST_INDENT_AFTER:
  case YES_FIRST_INDENT_AFTER:
  case PAGE_BREAK_BEFORE:
  case PAGE_BREAK:
  case NO_PAGE_BREAK_BEFORE:
  case NO_PAGE_BREAK_AFTER:
  case NEW_PAGE_BEFORE:
  case NEW_PAGE:
  case NEW_DOUBLE_PAGE_BEFORE:
  case NEW_DOUBLE_PAGE: r= replace (as_string (L(st)), "_", " "); break;
  case GROUP: r= "group"; break;
  case LEFT: r= "open#" * as_symbol (st[0]); break;
  case MIDDLE: r= "separator#" * as_symbol (st[0]); break;
  case RIGHT: r= "close#" * as_symbol (st[0]); break;
  case BIG: r= "big#" * as_symbol (st[0]); break;
  case LEFT_PRIME: r= "left prime#" * as_string (st[0]); break;
  case RIGHT_PRIME: r= "prime#" * as_string (st[0]); break;
  case BELOW: r= "below"; break;
  case ABOVE: r= "above"; break;
  case LEFT_SUB: r= "left subscript"; break;
  case LEFT_SUP: r= "left superscript"; break;
  case RIGHT_SUB: r= "subscript"; break;
  case RIGHT_SUP: r= "superscript"; break;
  case FRAC: r= "fraction"; break;
  case SQRT: r= (char*) ((N(st)==1)? "square root": "n-th root"); break;
  case WIDE: r=  get_accent_type (as_string (st[1])); break;
  case WIDE_UNDER: r= "under#" * get_accent_type (as_string (st[1])); break;
  case NEG: r= "negation"; break;
  case TREE: r= "tree"; break;
  case OLD_MATRIX: r= "old matrix"; break;
  case OLD_TABLE: r= "old table"; break;
  case OLD_MOSAIC: r= "old mosaic"; break;
  case TABLE_FORMAT: r= "table"; break;
  case TABLE_WITH: r= "table property"; break;
  case CELL_WITH: r= "cell property"; break;
  case TABLE_MARKER: r= "table marker"; break;
  case TABLE: r= "table"; break;
  case ROW: r= "row"; break;
  case CELL: r= "cell"; break;
  case SUB_TABLE: r= "subtable"; break;
  case ASSIGN: r= "assign#" * as_string (st[0]); break;
  case WITH: r= "with#" * get_with_text (st); break;
  case COMPOUND: r= "compound#" * as_string (st[0]); break;
  case INCLUDE: r= "include#" * as_string (st[0]); break;
  case MACRO: r= "macro"; break;
  case XMACRO: r= "xmacro"; break;
  case FUNCTION: r= "function"; break;
  case ENVIRONMENT: r= "environment"; break;
  case DRD_PROPS: r= "drd properties"; break;
  case EVAL: r= "evaluate"; break;
  case PROVIDES: r= "provides#" * as_string (st[0]); break;
  case VALUE: r= "value#" * as_string (st[0]); break;
  case ARGUMENT: r= "argument#" * as_string (st[0]); break;
  case GET_LABEL: r= "tree label"; break;
  case GET_ARITY: r= "arity"; break;
  case MAP_ARGS: r= "map arguments"; break;
  case EVAL_ARGS: r= "evaluate arguments"; break;
  case BACKUP: r= "backup"; break;
  case QUOTE: r= "quote"; break;
  case DELAY: r= "delay"; break;
  case HOLD: r= "hold"; break;
  case RELEASE: r= "release"; break;
  case OR: r= "or"; break;
  case XOR: r= "xor"; break;
  case AND: r= "and"; break;
  case NOT: r= "not"; break;
  case PLUS: r= "plus"; break;
  case MINUS: r= "minus"; break;
  case TIMES: r= "times"; break;
  case OVER: r= "over"; break;
  case DIVIDE: r= "divide"; break;
  case MODULO: r= "modulo"; break;
  case MERGE: r= "merge"; break;
  case LENGTH: r= "length"; break;
  case RANGE: r= "range"; break;
  case NUMBER: r= "number"; break;
  case _DATE: r= "date"; break;
  case TRANSLATE: r= "translate"; break;
  case FIND_FILE: r= "find file"; break;
  case IS_TUPLE: r= "tuple?"; break;
  case LOOK_UP: r= "look up"; break;
  case EQUAL: r= "equal"; break;
  case UNEQUAL: r= "not equal"; break;
  case LESS: r= "less"; break;
  case LESSEQ: r= "less or equal"; break;
  case GREATER: r= "greater"; break;
  case GREATEREQ: r= "greater or equal"; break;
  case IF: r= "if"; break;
  case VAR_IF: r= "if*"; break;
  case CASE: r= "case"; break;
  case WHILE: r= "while"; break;
  case EXTERN: r= "extern"; break;
  case AUTHORIZE: r= "authorize"; break;
  case INACTIVE: r= "inactive#" * as_string (st[0]); break;
  case ACTIVE: r= "active"; break;
  case VAR_INACTIVE: r= "inactive*" * as_string (st[0]); break;
  case VAR_ACTIVE: r= "active*"; break;
  case SYMBOL: r= "symbol"; break;
  case LATEX: r= "latex"; break;
  case HYBRID: r= "hybrid"; break;
  case TUPLE: r= "tuple"; break;
  case ATTR: r= "attr"; break;
  case COLLECTION: r= "collection"; break;
  case ASSOCIATE: r= "associate"; break;
  case LABEL: r= "label: " * as_string (st[0]); break;
  case REFERENCE: r= "reference: " * as_string (st[0]); break;
  case PAGEREF: r=  "page reference: " * as_string (st[0]); break;
  case WRITE: r= "write to " * as_string (st[0]); break;
  case SPECIFIC: r= "specific " * as_string (st[0]); break;
  case HYPERLINK: r= "hyperlink"; break;
  case ACTION: r= "action"; break;
  case TAG: r= "tag"; break;
  case MEANING: r= "meaning"; break;
  case FLAG: r= "flag"; break;
  case GRAPHICS: r= "graphics"; break;
  case SUPERPOSE: r= "superpose"; break;
  case TEXT_AT: r= "text"; break;
  case _POINT: r= "point"; break;
  case LINE: r= "line"; break;
  case CLINE: r= "cline"; break;
  case SPLINE: r= "spline"; break;
  case VAR_SPLINE: r= "var_spline"; break;
  case CSPLINE: r= "cspline"; break;
  case FILL: r= "fill"; break;
  case POSTSCRIPT: r= "postscript image"; break;
  default:
    if (L(st) < START_EXTENSIONS) r= as_string (mem_used ())*" bytes used";
    else r= as_string (L(st));
  }
  if (last_item (tp) == 0) r= "before#" * r;
  return r;
}

string
edit_interface_rep::compute_compound_footer (tree t, path p) {
  if (nil (p) || atom (p)) return "";
  string up= compute_compound_footer (t, path_up (p));
  tree st= subtree (t, path_up (p));
  int  l = last_item (p);
  switch (L (st)) {
  case RAW_DATA:
    return up * "raw data#";
  case SURROUND:
    if (l == 0) return up * "left surrounding#";
    if (l == 1) return up * "right surrounding#";
    return up;
  case MOVE:
    if (l==0) return up * "move#";
    else return up;
  case RESIZE:
    if (l==0) return up * "resize#";
    else return up;
  case _FLOAT:
    if (is_atomic (st[0])) return up * st[0]->label * "#";
    else return up * "float#";
  case REPEAT:
    return up * "repeat#";
  case DECORATE_ATOMS:
    return up * "decorate atoms#";
  case DECORATE_LINES:
    return up * "decorate lines#";
  case DECORATE_PAGES:
    return up * "decorate pages#";
  case GROUP:
    return up * "group#";
  case LEFT:
    return up * "left#";
  case MIDDLE:
    return up * "middle#";
  case RIGHT:
    return up * "right#";
  case BIG:
    return up * "big#";
  case BELOW:
    if (l==0) return up * "body#";
    else return up * "script below#";
  case ABOVE:
    if (l==0) return up * "body#";
    else return up * "script above#";
  case LEFT_SUB:
    return up * "left subscript#";
  case LEFT_SUP:
    return up * "left superscript#";
  case RIGHT_SUB:
    return up * "subscript#";
  case RIGHT_SUP:
    return up * "superscript#";
  case FRAC:
    if (l==0) return up * "numerator#";
    else return up * "denominator#";
  case SQRT:
    if (N(st)==1) return up * "square root#";
    if (l==0) return up * "root#";
    else return up * "index#";
  case WIDE:
    return up * get_accent_type (as_string (st[1])) * "#";
  case WIDE_UNDER:
    return up * "under#" * get_accent_type (as_string (st[1])) * "#";
  case NEG:
    return up * "negation#";
  case TREE:
    if (l==0) return up * "root#";
    else return up * "branch(" * as_string (l) * ")#";
  case OLD_MATRIX:
    return up * "old matrix#";
  case OLD_TABLE:
    return up * "old table#";
  case OLD_MOSAIC:
    return up * "old mosaic#";
  case TABLE_FORMAT:
    return up;
  case TABLE_WITH:
    return up * "table property#";
  case CELL_WITH:
    return up * "cell property#";
  case TABLE_MARKER:
    return up * "table marker#";
  case TABLE:
    return up * "(" * as_string (l+1) * ",";
  case ROW:
    return up * as_string (l+1) * ")#";
  case CELL:
    return up;
  case SUB_TABLE:
    return up * "subtable#";
  case COMPOUND:
    return up * as_string (st[0]) * "#";
  case WITH:
    return up * get_with_text (st) * "#";
  case INCLUDE:
    return up * "include#";
  case DRD_PROPS:
    if (l == 0) return up * "drd property(variable)" * "#";
    if ((l&1) == 1) return up * "drd property(" * as_string (l/2+1) * ")#";
    return up * "value(" * as_string (l/2) * ")#";
  case EVAL:
    return up * "evaluate#";
  case PROVIDES:
    return up * "provides#";
  case VALUE:
    return up * "value#";
  case ARGUMENT:
    return up * "argument#";
  case GET_LABEL:
    return up * "tree label#";
  case GET_ARITY:
    return up * "arity#";
  case MAP_ARGS:
    return up * "map arguments#";
  case EVAL_ARGS:
    return up * "evaluate arguments#";
  case QUOTE:
    return up * "quote#";
  case DELAY:
    return up * "delay#";
  case SYMBOL:
    return up * "symbol#";
  case HOLD:
    return up * "hold#";
  case RELEASE:
    return up * "release#";
  case LATEX:
    return up * "latex#";
  case HYBRID:
    return up * "hybrid#";
  case TUPLE:
    return up * "tuple(" * as_string (l+1) * ")#";
  case ATTR:
    if ((l&1) == 0) return up * "variable(" * as_string (l/2+1) * ")#";
    else return up * "value(" * as_string (l/2+1) * ")#";
  case SPECIFIC:
    return up * "texmacs#";
  case HYPERLINK:
    return up * "hyperlink(" * as_string (st[1]) * ")#";
  case ACTION:
    return up * "action#";
  case TAG:
    return up * "tag#";
  case MEANING:
    return up * "meaning#";
  case FLAG:
    return up * "flag#";
  default:
    if (L(st) < START_EXTENSIONS) return up;
    else return up * as_string (L(st)) * "#";
  }
}

void
edit_interface_rep::set_right_footer () {
  string s, r;
  tree st= subtree (et, path_up (tp));
  if (is_atomic (st)) r= compute_text_footer (st);
  else r= compute_operation_footer (st);
  r= compute_compound_footer (et, path_up (tp)) * r;
  set_right_footer (r);
}

/******************************************************************************
* Set footer with information about execution of latex command
******************************************************************************/

bool
edit_interface_rep::set_latex_footer (tree st) {
  if (is_atomic (st)) 
    if (is_func (subtree (et, path_up (path_up (tp))), LATEX, 1) ||
	is_func (subtree (et, path_up (path_up (tp))), HYBRID, 1)) {
      string s= st->label;
      string help;
      command cmd;
      if (sv->kbd_get_command (s, help, cmd)) {
	set_left_footer ("return:#" * help);
	set_right_footer ("latex command");
	return true;
      }
    }
  return false;
}

/******************************************************************************
* Update footer
******************************************************************************/

DEBUG
(
  int concrete_count = 0;
  int abstract_count = 0;
  int box_count      = 0;
  int event_count    = 0;
  int widget_count   = 0;
  int line_item_count= 0;
  int list_count     = 0;
  int command_count  = 0;
  int iterator_count = 0;
  int function_count = 0;
  int instance_count = 0;
)

void
edit_interface_rep::set_footer () {
  DEBUG
  (
    cout << "--------------------------------------------------------------\n";
    cout << "concrete  " << concrete_count << "\n";
    cout << "abstract  " << abstract_count << "\n";
    cout << "widget    " << widget_count << "\n";
    cout << "box       " << box_count << "\n";
    cout << "event     " << event_count << "\n";
    cout << "line item " << line_item_count << "\n";
    cout << "list      " << list_count << "\n";
    cout << "command   " << command_count << "\n";
    cout << "iterator  " << iterator_count << "\n";
    cout << "function  " << function_count << "\n";
    cout << "instance  " << instance_count << "\n";
  )

  if ((N(message_l) == 0) && (N(message_r) == 0)) {
    tree st= subtree (et, path_up (tp));
    if (set_latex_footer (st)) return;
    set_left_footer();
    set_right_footer();
  }
  else {
    set_left_footer (message_l);
    set_right_footer (message_r);
    message_l= message_r= "";
  }
}

/******************************************************************************
* Interactive commands
******************************************************************************/

class interactive_command_rep: public command_rep {
  edit_interface_rep* ed;
  scheme_tree p;    // the interactive arguments
  scheme_tree q;    // the function which is applied to the arguments
  int         i;    // counter where we are
  string*     s;    // feedback from interaction with user

public:
  interactive_command_rep (
    edit_interface_rep* Ed, scheme_tree P, scheme_tree Q):
      ed (Ed), p (P), q (Q), i (0), s (new string [N(p)]) {}
  ~interactive_command_rep () { delete[] s; }
  void apply ();
  ostream& print (ostream& out) {
    return out << "interactive command " << p; }
};

void
interactive_command_rep::apply () {
  if ((i>0) && (s[i-1] == "cancel")) return;
  if (i == arity (p)) {
    tree prg (TUPLE, N(p)+1);
    prg[0]= q;
    for (i=0; i<N(p); i++) prg[i+1]= s[i];
    string ret= as_string (eval (scheme_tree_to_string (prg)));
    if ((ret != "") && (ret != "#<unspecified>"))
      ed->set_message (ed->message_l, "interactive command");
  }
  else {
    if ((!is_atomic (p[i])) || (!is_quoted (p[i]->label))) return;
    s[i]= string ("");
    tm_view temp_vw= ed->sv->get_view (false);
    ed->focus_on_this_editor ();
    ed->sv->interactive (unquote (p[i]->label), s[i], this);
    ed->sv->set_view (temp_vw);
    i++;
  }
}

/******************************************************************************
* Exported routines
******************************************************************************/

void
edit_interface_rep::set_message (string l, string r) {
  message_l= l;
  message_r= r;
  notify_change (THE_DECORATIONS);
}

void
edit_interface_rep::interactive (scheme_tree p, scheme_tree q) {
  if (!is_tuple (p))
    fatal_error ("tuple expected", "edit_interface_rep::interactive");
  command interactive_cmd= new interactive_command_rep (this, p, q);
  interactive_cmd ();
}
