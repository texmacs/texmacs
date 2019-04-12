
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
#include "dictionary.hpp"

/******************************************************************************
* Set left footer with information about environment variables
******************************************************************************/

void
edit_interface_rep::set_left_footer (tree l) {
  SERVER (set_left_footer (translate (l)));
}

void
edit_interface_rep::append_left_footer (tree& l, string env_var) {
  if (!is_concat (l)) l= concat (l);
  string i= get_init_string (env_var);
  string c= get_env_string (env_var);
  if (c != i) l << (" " * c);
}

void
edit_interface_rep::set_left_footer () {
  tree s= concat ();
  double base_sz= get_env_int (FONT_BASE_SIZE);
  double sz= get_env_double (FONT_SIZE);
  /*
  tree the_style= get_style ();
  for (int i=0; i<arity (the_style); i++)
    s << " " << as_string (the_style[i]);
  */
  string mode= get_env_string (MODE);
  string lan = get_env_string (MODE_LANGUAGE (mode));
  if (mode == "prog") s << "program";
  else if (as_string (get_init_value (MODE_LANGUAGE (mode))) != lan)
    s << " " << lan;
  else s << " " << mode;
  if ((mode == "text") || (mode == "src")) {
    s << " " << main_family (get_env_string (FONT));
    append_left_footer (s, FONT_FAMILY);
    s << " " << as_string ((int) ((base_sz+0.5)*sz));
    append_left_footer (s, FONT_SERIES);
    append_left_footer (s, FONT_SHAPE);
  }
  else if (mode == "math") {
    s << " " << get_env_string (MATH_FONT);
    append_left_footer (s, MATH_FONT_FAMILY);
    s << " " << as_string ((int) ((base_sz+0.5)*sz));
    append_left_footer (s, MATH_FONT_SERIES);
    append_left_footer (s, MATH_FONT_SHAPE);
  }
  else if (mode == "prog") {
    string session_name= get_env_string (PROG_SESSION);
    if (session_name != "default") s << "-" << session_name;
    s << " " << get_env_string (PROG_FONT);
    append_left_footer (s, PROG_FONT_FAMILY);
    s << " " << as_string ((int) ((base_sz+0.5)*sz));
    append_left_footer (s, PROG_FONT_SERIES);
    append_left_footer (s, PROG_FONT_SHAPE);
  }
  string r= get_env_string (COLOR);
  if (r != "black") s << " " << r;
  if (N(s) > 0 && s[0] == " ") s= s (1, N(s));
  if (inside ("session") && (lan != "scheme")) {
    string lan    = get_env_string (PROG_LANGUAGE);
    string session= get_env_string (PROG_SESSION);
    switch (connection_status (lan, session)) {
    case CONNECTION_DEAD:
      s= s << " [dead]";
      break;
    case CONNECTION_DYING:
    case WAITING_FOR_OUTPUT:
      s= s << " [busy]";
      break;
    case WAITING_FOR_INPUT:
      s= s << " [idle]";
      break;
    }
  }
  s= as_tree (call ("footer-hook", object (s)));
  set_left_footer (s);
}

/******************************************************************************
* Set right footer with information about cursor position
******************************************************************************/

void
edit_interface_rep::set_right_footer (tree r) {
  SERVER (set_right_footer (translate (r)));
}

tree
edit_interface_rep::compute_text_footer (tree st) {
  string r;
  language lan= get_env_language ();
  int end  = last_item (tp);
  int start= end;
  tm_char_backwards (st->label, start);
  r= st->label (start, end);
  if (r == "") r= "start";
  if (r == " ") r= "space";
  if (r == "space" && get_env_string (MODE) == "math") r= "apply";
  if (starts (r, "<") && !starts (r, "<#"))
    if (cork_to_utf8 (r) != r)
      r= r * " (" * r(1, N(r)-1) * ")";
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

static tree
get_with_text (tree t) {
  int i, n=N(t), k=(n-1)/2;
  if ((n&1)!=1) return "";
  if (is_func (t[n-1], GRAPHICS)) return "";
  tree s= concat ();
  for (i=0; i<k; i++)
    if (is_atomic (t[2*i]) && (t[2*i]!="") && is_atomic (t[2*i+1])) {
      if (i>0) s << " ";
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
	s << (var * "=" * t[2*i+1]->label);
      else s << t[2*i+1]->label;
    }
  return s;
}

tree
edit_interface_rep::compute_operation_footer (tree st) {
  tree r= "";
  if (N(st) >= 2) {
    switch (L (st)) {
    case VAR_WIDE:
      r= concat ("under ", get_accent_type (as_string (st[1]))); break;
    default: ;
    }
  }
  if (r == "" && N(st) >= 1) {
    switch (L (st)) {
    case HSPACE:
      r= concat ("space"); break;
    case VAR_VSPACE:
      r= concat ("vertical space before");
      break;
    case VSPACE:
      r= concat ("vertical space"); break;
    case SPACE:
      r= concat ("space"); break;
    case _FLOAT:
      r= (is_atomic (st[0])? st[0]->label: string ("float")); break;
    case MID:
      r= concat ("middle ", as_symbol (st[0])); break;
    case RIGHT:
      r= concat ("close ", as_symbol (st[0])); break;
    case BIG:
      r= concat ("big ", as_symbol (st[0])); break;
    case LPRIME:
      r= concat ("left prime ", as_string (st[0])); break;
    case LONG_ARROW:
      r= concat ("long arrow ", as_string (st[0])); break;
    case RPRIME:
      r= concat ("prime ", as_string (st[0])); break;
    case SQRT:
      r= tree ((char*) ((N(st)==1)? "square root": "n-th root")); break;
    case WIDE:
      r= tree (get_accent_type (as_string (st[1]))); break;
    case ASSIGN:
      r= concat ("assign ", as_string (st[0])); break;
    case PROVIDE:
      r= concat ("provide ", as_string (st[0])); break;
    case WITH:
      r= concat ("with ", get_with_text (st)); break;
    case PROVIDES:
      r= concat ("provides ", as_string (st[0])); break;
    case VALUE:
      r= concat ("value ", as_string (st[0])); break;
    case QUOTE_VALUE:
      r= concat ("quoted value ", as_string (st[0])); break;
    case OR_VALUE:
      r= concat ("or-value ", as_string (st[0])); break;
    case ARG:
      r= concat ("argument ", as_string (st[0])); break;
    case QUOTE_ARG:
      r= concat ("quoted argument ", as_string (st[0])); break;
    case COMPOUND:
      if (is_atomic (st[0])) r= as_string (st[0]);
      else r= "compound";
      break;
    case VAR_INCLUDE:
    case INCLUDE:
      r= concat ("include ", as_string (st[0])); break;
    case INACTIVE:
      r= concat ("inactive ", drd->get_name (L(st[0]))); break;
    case VAR_INACTIVE:
      r= concat ("inactive ", drd->get_name (L(st[0]))); break;
    case LABEL:
      r= concat ("label: ", as_string (st[0])); break;
    case REFERENCE:
      r= concat ("reference: ", as_string (st[0])); break;
    case PAGEREF:
      r= concat ("page reference: ", as_string (st[0])); break;
    case GET_ATTACHMENT:
      r= concat ("get attachment: ", as_string (st[0])); break;
    case WRITE:
      r= concat ("write to ", as_string (st[0])); break;
    case TOC_NOTIFY:
      r= concat ("toc notify: ", as_string (st[1])); break;
    case SPECIFIC:
      r= concat ("specific ", as_string (st[0])); break;
    case HYPHENATE_AS:
      r= concat ("hyphenate as ", as_string (st[0])); break;
    case IMAGE:
      r= concat ("image"); break;
    default: ;
    }
  }
  if (r == "") {
    switch (L (st)) {
    case IMAGE: r= "image"; break;
    default: r= drd->get_name (L(st));
    }
  }
  if (last_item (tp) == 0) r= concat ("before ", r);
  return r;
}

tree
edit_interface_rep::compute_compound_footer (tree t, path p) {
  if (!(rp < p)) return "";
  tree up= compute_compound_footer (t, path_up (p));
  if (N(focus_get (false))+1 < N(p)) return up;
  tree st= subtree (t, path_up (p));
  int  l = last_item (p);
  switch (L (st)) {
  case DOCUMENT:
  case PARA:
    return up;
  case SURROUND:
    if (l == 0) return concat (up, "left surrounding ");
    if (l == 1) return concat (up, "right surrounding ");
    return up;
  case CONCAT:
    return up;
  case MOVE:
    if (l == 0) return concat (up, "move ");
    else return up;
  case SHIFT:
    if (l==0) return concat (up, "shift ");
    else return up;
  case RESIZE:
    if (l==0) return concat (up, "resize ");
    else return up;
  case CLIPPED:
    if (l==0) return concat (up, "clipped ");
    else return up;
  case _FLOAT:
    if (N(st) >= 1 && is_atomic (st[0]))
      return concat (up, st[0]->label * " ");
    else return concat (up, "float ");
  case LONG_ARROW:
    if (l == 1) return concat (up, "above ");
    else if (l == 2) return concat (up, "below ");
    else return up;
  case BELOW:
    if (l==0) return concat (up, "body ");
    else return concat (up, "script below ");
  case ABOVE:
    if (l==0) return concat (up, "body ");
    else return concat (up, "script above ");
  case FRAC:
    if (l==0) return concat (up, "numerator ");
    else return concat (up, "denominator ");
  case SQRT:
    if (N(st)==1) return concat (up, "square root ");
    if (l==0) return concat (up, "root ");
    else return concat (up, "index ");
  case WIDE:
    if (N(st) >= 1)
      return concat (up, get_accent_type (as_string (st[1])) * " ");
    else
      return concat (up, "wide ");
  case VAR_WIDE:
    if (N(st) >= 1)
      return concat (up, "under " * get_accent_type (as_string (st[1])) * " ");
    else
      return concat (up, "var-wide ");
  case TREE:
    if (l==0) return concat (up, "root ");
    else return concat (up, "branch(" * as_string (l) * ") ");
  case TFORMAT:
    return up;
  case TABLE:
    return concat (up, "(" * as_string (l+1) * ",");
  case ROW:
    return concat (up, as_string (l+1) * ") ");
  case CELL:
    return up;
  case WITH:
    return concat (up, get_with_text (st), " ");
  case DRD_PROPS:
    if (l == 0)
      return concat (up, "drd property(variable) ");
    if ((l&1) == 1)
      return concat (up, "drd property(" * as_string (l/2+1) * ") ");
    return concat (up, "value(" * as_string (l/2) * ") ");
  case COMPOUND:
    if (N(st) >= 1 && is_atomic (st[0]))
      return concat (up, as_string (st[0]) * " ");
    else
      return concat (up, "compound ");
  case HLINK:
    if (N(st) >= 2) {
      if (l == 0) {
        return concat (up, "hyperlink(text) ");
      } else {
        return concat (up, "hyperlink(destination) ");
      }
    } else {
      return concat (up, "hyperlink ");
    }
  case TUPLE:
    return concat (up, "tuple(" * as_string (l+1) * ") ");
  case ATTR:
    if ((l&1) == 0)
      return concat (up, "variable(" * as_string (l/2+1) * ") ");
    else
      return concat (up, "value(" * as_string (l/2+1) * ") ");
  case SPECIFIC:
    return concat (up, "texmacs ");
  case ANIM_CONSTANT:
    if (l == 0) return concat (up, "anim-constant ");
    else return up;
  case ANIM_ACCELERATE:
    if (l == 0 && N(st) == 2) return concat (up, as_string (st[1]) * " ");
    else return up;
  case ANIM_TRANSLATE:
    if (l == 0)
      return concat (up, "anim-translate ");
    else return up;
  case ANIM_PROGRESSIVE:
    if (l == 0)
      return concat (up, "anim-progressive ");
    else return up;
  default:
    return concat (up, drd->get_name (L(st)) * " ");
  }
}

void
edit_interface_rep::set_right_footer () {
  tree cf= compute_compound_footer (et, path_up (tp));
  tree st= subtree (et, path_up (tp));
  tree lf;
  if (is_atomic (st)) lf= compute_text_footer (st);
  else lf= compute_operation_footer (st);
  if (N(focus_get (false))+1 >= N(tp)) cf= concat (cf, lf);
  set_right_footer (cf);
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
	set_left_footer (concat (kbd ("return"), ": " * help));
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
	    set_message (concat (kbd ("return"), ": insert argument ", name),
			 "hybrid command");
	    return true;
	  }
      }
      // macro application
      tree f= get_env_value (name);
      if (drd->contains (name) && (f == UNINIT))
	set_message (concat (kbd ("return"), ": insert primitive ", name),
		     "hybrid command");
      else if (is_func (f, MACRO) || is_func (f, XMACRO))
	set_message (concat (kbd ("return"), ": insert macro ", name),
		     "hybrid command");
      else if (f != UNINIT)
	set_message (concat (kbd ("return"), ": insert value ", name),
		     "hybrid command");
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
    else set_left_footer (message_l);
    if (message_r == "") set_right_footer ();
    else set_right_footer (message_r);
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
