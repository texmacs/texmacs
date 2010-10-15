
/******************************************************************************
* MODULE     : edit_footer.cpp
* DESCRIPTION: display interesting information for the user in the footer
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "edit_interface.hpp"
#include "convert.hpp"
#include "connect.hpp"

/******************************************************************************
* Convert structured messages into straight messages
******************************************************************************/

string
edit_interface_rep::flatten_message (tree t, bool localize) {
  if (is_atomic (t)) return t->label;
  else if (is_concat (t)) {
    string s;
    for (int i=0; i<N(t); i++) {
      tree u= t[i];
      while (is_concat (u) && N(u) > 0) u= u[0];
      if (i > 0 && is_compound (u, "render-key")) {
	if (use_macos_fonts () || gui_is_qt ()) s << "  ";
	else s << " ";
      }
      s << flatten_message (t[i], localize);
    }
    return s;
  }
  else if (is_compound (t, "verbatim", 1))
    return flatten_message (t[0], false);
  else if (is_compound (t, "localize", 1))
    return flatten_message (t[0], true);
  else if (is_compound (t, "render-key", 1))
    return flatten_message (t[0], localize);
  else if (is_func (t, WITH))
    return flatten_message (t[N(t)-1], localize);
  else {
    cout << "TeXmacs] warning, bad message: " << t << "\n";
    return "";
  }
}

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
  string lan = get_env_string (MODE_LANGUAGE (mode));
  if (mode == "prog") s= s * "#program";
  else if (as_string (get_init_value (MODE_LANGUAGE (mode))) != lan)
    s= s * "#" * lan;
  else s= s * "#" * mode;
  if ((mode == "text") || (mode == "src")) {
    s= s * "#" * get_env_string (FONT);
    append_left_footer (s, FONT_FAMILY);
    s= s * "#" * as_string ((int) ((base_sz+0.5)*sz));
    append_left_footer (s, FONT_SERIES);
    append_left_footer (s, FONT_SHAPE);
  }
  else if (mode == "math") {
    s= s * "#" * get_env_string (MATH_FONT);
    append_left_footer (s, MATH_FONT_FAMILY);
    s= s * "#" * as_string ((int) ((base_sz+0.5)*sz));
    append_left_footer (s, MATH_FONT_SERIES);
    append_left_footer (s, MATH_FONT_SHAPE);
  }
  else if (mode == "prog") {
    string session_name= get_env_string (PROG_SESSION);
    if (session_name != "default") s= s * "-" * session_name;
    s= s * "#" * get_env_string (PROG_FONT);
    append_left_footer (s, PROG_FONT_FAMILY);
    s= s * "#" * as_string ((int) ((base_sz+0.5)*sz));
    append_left_footer (s, PROG_FONT_SERIES);
    append_left_footer (s, PROG_FONT_SHAPE);
  }
  r= get_env_string (COLOR);
  if (r != "black") s= s * "#" * r;
  if ((N(s)>0) && (s[0] == '#')) s= s (1, N(s));
  if (inside ("session") && (lan != "scheme")) {
    string lan    = get_env_string (PROG_LANGUAGE);
    string session= get_env_string (PROG_SESSION);
    switch (connection_status (lan, session)) {
    case CONNECTION_DEAD:
      s= s * "#[dead]";
      break;
    case CONNECTION_DYING:
    case WAITING_FOR_OUTPUT:
      s= s * "#[busy]";
      break;
    case WAITING_FOR_INPUT:
      s= s * "#[idle]";
      break;
    }
  }
  s= as_string (call ("footer-hook", object (s)));
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
  tm_char_backwards (st->label, start);
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
	  (var!=LANGUAGE) && (var!=FONT) &&
	  (var!=FONT_FAMILY) && (var!=FONT_SHAPE) && (var!=FONT_SERIES) &&
	  (var!=MATH_LANGUAGE) && (var!=MATH_FONT) &&
	  (var!=MATH_FONT_FAMILY) && (var!=MATH_FONT_SHAPE) &&
	  (var!=MATH_FONT_SERIES) &&
	  (var!=PROG_LANGUAGE) && (var!=PROG_FONT) &&
	  (var!=PROG_FONT_FAMILY) && (var!=PROG_FONT_SHAPE) &&
	  (var!=PROG_FONT_SERIES) &&
	  (var!=PROG_SESSION))
	s << var << "=";
      s << t[2*i+1]->label;
    }
  return s;
}

string
edit_interface_rep::compute_operation_footer (tree st) {
  string r = "";
  if (N(st) >= 2) {
    switch (L (st)) {
    case VAR_WIDE: r= "under#" * get_accent_type (as_string (st[1])); break;
    default: ;
    }
  }
  if (r == "" && N(st) >= 1) {
    switch (L (st)) {
    case _FLOAT: r= (is_atomic (st[0])? st[0]->label: string ("float")); break;
    case MID: r= "middle#" * as_symbol (st[0]); break;
    case RIGHT: r= "close#" * as_symbol (st[0]); break;
    case BIG: r= "big#" * as_symbol (st[0]); break;
    case LPRIME: r= "left prime#" * as_string (st[0]); break;
    case RPRIME: r= "prime#" * as_string (st[0]); break;
    case SQRT: r= (char*) ((N(st)==1)? "square root": "n-th root"); break;
    case WIDE: r=  get_accent_type (as_string (st[1])); break;
    case ASSIGN: r= "assign#" * as_string (st[0]); break;
    case WITH: r= "with#" * get_with_text (st); break;
    case PROVIDES: r= "provides#" * as_string (st[0]); break;
    case VALUE: r= "value#" * as_string (st[0]); break;
    case QUOTE_VALUE: r= "quoted value#" * as_string (st[0]); break;
    case ARG: r= "argument#" * as_string (st[0]); break;
    case QUOTE_ARG: r= "quoted argument#" * as_string (st[0]); break;
    case COMPOUND:
      if (is_atomic (st[0])) r= as_string (st[0]);
      else r= "compound";
      break;
    case INCLUDE: r= "include#" * as_string (st[0]); break;
    case INACTIVE: r= "inactive#" * drd->get_name (L(st[0])); break;
    case VAR_INACTIVE: r= "inactive#" * drd->get_name (L(st[0])); break;
    case LABEL: r= "label: " * as_string (st[0]); break;
    case REFERENCE: r= "reference: " * as_string (st[0]); break;
    case PAGEREF: r=  "page reference: " * as_string (st[0]); break;
    case WRITE: r= "write to " * as_string (st[0]); break;
    case SPECIFIC: r= "specific " * as_string (st[0]); break;
    default: ;
    }
  }
  if (r == "") {
    switch (L (st)) {
    case POSTSCRIPT: r= "postscript image"; break;
    default: r= drd->get_name (L(st));
    }
  }
  if (last_item (tp) == 0) r= "before#" * r;
  return r;
}

string
edit_interface_rep::compute_compound_footer (tree t, path p) {
  if (!(rp < p)) return "";
  string up= compute_compound_footer (t, path_up (p));
  tree st= subtree (t, path_up (p));
  int  l = last_item (p);
  switch (L (st)) {
  case DOCUMENT:
  case PARA:
    return up;
  case SURROUND:
    if (l == 0) return up * "left surrounding#";
    if (l == 1) return up * "right surrounding#";
    return up;
  case CONCAT:
    return up;
  case MOVE:
    if (l==0) return up * "move#";
    else return up;
  case RESIZE:
    if (l==0) return up * "resize#";
    else return up;
  case _FLOAT:
    if (N(st) >= 1 && is_atomic (st[0])) return up * st[0]->label * "#";
    else return up * "float#";
  case BELOW:
    if (l==0) return up * "body#";
    else return up * "script below#";
  case ABOVE:
    if (l==0) return up * "body#";
    else return up * "script above#";
  case FRAC:
    if (l==0) return up * "numerator#";
    else return up * "denominator#";
  case SQRT:
    if (N(st)==1) return up * "square root#";
    if (l==0) return up * "root#";
    else return up * "index#";
  case WIDE:
    if (N(st) >= 1) return up * get_accent_type (as_string (st[1])) * "#";
    else return up * "wide#";
  case VAR_WIDE:
    if (N(st) >= 1)
      return up * "under#" * get_accent_type (as_string (st[1])) * "#";
    else return up * "var-wide#";
  case TREE:
    if (l==0) return up * "root#";
    else return up * "branch(" * as_string (l) * ")#";
  case TFORMAT:
    return up;
  case TABLE:
    return up * "(" * as_string (l+1) * ",";
  case ROW:
    return up * as_string (l+1) * ")#";
  case CELL:
    return up;
  case WITH:
    return up * get_with_text (st) * "#";
  case DRD_PROPS:
    if (l == 0) return up * "drd property(variable)" * "#";
    if ((l&1) == 1) return up * "drd property(" * as_string (l/2+1) * ")#";
    return up * "value(" * as_string (l/2) * ")#";
  case COMPOUND:
    if (N(st) >= 1 && is_atomic (st[0])) return up * as_string (st[0]) * "#";
    else return up * "compound#";
  case HLINK:
    if (N(st) >= 2) return up * "hyperlink(" * as_string (st[1]) * ")#";
    else return up * "#hyperlink";
  case TUPLE:
    return up * "tuple(" * as_string (l+1) * ")#";
  case ATTR:
    if ((l&1) == 0) return up * "variable(" * as_string (l/2+1) * ")#";
    else return up * "value(" * as_string (l/2+1) * ")#";
  case SPECIFIC:
    return up * "texmacs#";
  default:
    return up * drd->get_name (L(st)) * "#";
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
    if (is_func (subtree (et, path_up (tp, 2)), LATEX, 1) ||
	is_func (subtree (et, path_up (tp, 2)), HYBRID, 1)) {
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

bool
edit_interface_rep::set_hybrid_footer (tree st) {
  // WARNING: update edit_dynamic_rep::activate_hybrid when updating this
  if (is_atomic (st))
    if (is_func (subtree (et, path_up (tp, 2)), HYBRID, 1)) {
      string msg;
      // macro argument
      string name= st->label;
      path mp= search_upwards (MACRO);
      if (!is_nil (mp)) {
	tree mt= subtree (et, mp);
	int i, n= N(mt)-1;
	for (i=0; i<n; i++)
	  if (mt[i] == name) {
	    set_message ("return:#insert argument#" * name, "hybrid command");
	    return true;
	  }
      }
      // macro application
      tree f= get_env_value (name);
      if (drd->contains (name) && (f == UNINIT))
	set_message("return:#insert primitive#" * name, "hybrid command");
      else if (is_func (f, MACRO) || is_func (f, XMACRO))
	set_message("return:#insert macro#" * name, "hybrid command");
      else if (f != UNINIT)
	set_message("return:#insert value#" * name, "hybrid command");
      else return false;
      return true;
    }
  return false;
}

/******************************************************************************
* Update footer
******************************************************************************/

TM_DEBUG
(
  int concrete_count = 0;
  int abstract_count = 0;
  int box_count      = 0;
  int event_count    = 0;
  int widget_count   = 0;
  int line_item_count= 0;
  int list_count     = 0;
  int command_count  = 0;
  int observer_count = 0;
  int iterator_count = 0;
  int function_count = 0;
  int instance_count = 0;
)

void
edit_interface_rep::set_footer () {
  TM_DEBUG
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
    cout << "observer  " << observer_count << "\n";
    cout << "iterator  " << iterator_count << "\n";
    cout << "function  " << function_count << "\n";
    cout << "instance  " << instance_count << "\n";
  )

  if ((message_l == "") && (message_r == "")) {
    last_l= ""; last_r= "";
    tree st= subtree (et, path_up (tp));
    if (set_latex_footer (st)) return;
    if (set_hybrid_footer (st)) return;
    set_left_footer();
    set_right_footer();
  }
  else {
    if (message_l == "") set_left_footer ();
    else set_left_footer (flatten_message (message_l));
    if (message_r == "") set_right_footer ();
    else set_right_footer (flatten_message (message_r));
    message_l= message_r= "";
  }
}

/******************************************************************************
* Exported routines
******************************************************************************/

void
edit_interface_rep::set_message (tree l, tree r, bool temp) {
  eval ("(set-message-notify)");
  message_l= l;
  message_r= r;
  if (!temp) {
    last_l= l;
    last_r= r;
  }
  notify_change (THE_DECORATIONS);
}

void
edit_interface_rep::recall_message () {
  set_message (last_l, last_r);
}
