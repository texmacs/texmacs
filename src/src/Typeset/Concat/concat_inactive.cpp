
/******************************************************************************
* MODULE     : concat_inactive.cpp
* DESCRIPTION: Typesetting of inactive markup
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "concater.hpp"

/******************************************************************************
* Typesetting deactivated trees
******************************************************************************/

void
concater_rep::typeset_inactive_tag (tree t, path ip) { 
  marker (descend (ip, 0));
  typeset (t[0], descend (ip, 0), false);
  marker (descend (ip, 1));
}

void
concater_rep::typeset_eval_args (tree t, path ip) { 
  marker (descend (ip, 0));
  typeset (env->exec (t), decorate_right (ip), false);
  marker (descend (ip, 1));
}

void
concater_rep::typeset_inactive_expand (
  string type, tree t, path ip, int pos1, int pos2)
{
  int i, j;
  tree old_col;
  penalty_min (0);
  marker (descend (ip, 0));
  ghost ("<", descend (ip, 0));
  ghost (type, descend (ip, 0));
  for (i=0; i<N(t); i++) {
    print (space (0, 0, env->fn->spc->max));
    int start= N(a);
    print (space (0, 0, env->fn->spc->max));
    ghost ("|", descend (descend (ip, i), 0));
    print (space (0, 0, env->fn->spc->max));
    if (i<N(t)-1) penalty_min (0);
    if (i<pos1) old_col= env->local_begin (COLOR, "dark green");
    typeset (t[i], descend (ip, i));
    if (i<pos1) env->local_end (COLOR, old_col);
    // ghost ("}", descend (descend (ip, i), right_index (t[i])));
    int end= N(a);
    if (i<pos2) for (j=start; j<end; j++)
      a[j]->b->relocate (decorate_left (ip), true);
  }
  if (i<=pos2) ghost (">", descend (ip, 1));
  else ghost (">", descend (descend (ip, i-1), right_index (t[i-1])));
  marker (descend (ip, 1));
  print (space (0, 0, env->fn->spc->max));
  penalty_min (0);
}

void
concater_rep::typeset_inactive_symbol (tree t, path ip) {
  penalty_min (0);
  marker (descend (ip, 0));
  ghost ("<", descend (ip, 0));
  tree old_col= env->local_begin (COLOR, "blue");
  typeset (t[0], descend (ip, 0));
  env->local_end (COLOR, old_col);
  ghost (">", descend (ip, 1));
  marker (descend (ip, 1));
  print (space (0, 0, env->fn->spc->max));
  penalty_min (0);
}

void
concater_rep::typeset_inactive_latex (tree t, path ip) {
  penalty_min (0);
  marker (descend (ip, 0));
  ghost ("\\", descend (ip, 0));
  tree old_col= env->local_begin (COLOR, "dark green");
  typeset (t[0], descend (ip, 0));
  env->local_end (COLOR, old_col);
  marker (descend (ip, 1));
  print (space (0, 0, env->fn->spc->max));
  penalty_min (0);
}

void
concater_rep::typeset_inactive_hybrid (tree t, path ip) {
  penalty_min (0);
  marker (descend (ip, 0));
  ghost ("<", descend (ip, 0));
  ghost ("\\", descend (ip, 0));
  tree old_col= env->local_begin (COLOR, "dark green");
  typeset (t[0], descend (ip, 0));
  env->local_end (COLOR, old_col);
  if (N(t) == 2) {
    print (space (0, 0, env->fn->spc->max));
    ghost ("|", descend (descend (ip, 1), 0));
    print (space (0, 0, env->fn->spc->max));
    typeset (t[1], descend (ip, 1));
  }
  if (N(t) == 0) ghost (">", descend (ip, 1));
  else ghost (">", descend (descend (ip, N(t)-1), right_index (t[N(t)-1])));
  marker (descend (ip, 1));
  print (space (0, 0, env->fn->spc->max));
  penalty_min (0);
}

void
concater_rep::typeset_inactive_specific (tree t, path ip) {
  bool flag= (t[0] != "texmacs") && (t[0] != "screen") && (t[0] != "printer");
  string mode, var, value;
  if (flag) {
    mode= env->get_string (MODE);
    if (mode == "text") { var=FONT_FAMILY; value="tt"; }
    else if (mode == "math") { var=MATH_FONT_FAMILY; value="mt"; }
    else { var=PROG_FONT_FAMILY; value="tt"; }
  }

  penalty_min (0);
  marker (descend (ip, 0));
  ghost ("<", descend (ip, 0));
  tree old_col= env->local_begin (COLOR, "dark green");
  ghost ("specific", descend (ip, 0));
  env->local_end (COLOR, old_col);
  print (space (0, 0, env->fn->spc->max));
  ghost ("|", descend (descend (ip, 0), 0));
  print (space (0, 0, env->fn->spc->max));
  typeset (t[0], descend (ip, 0));
  print (space (0, 0, env->fn->spc->max));
  ghost ("|", descend (descend (ip, 1), 0));
  print (space (0, 0, env->fn->spc->max));
  tree old;
  if (flag) old= env->local_begin (var, value);
  typeset (t[1], descend (ip, 1));
  if (flag) env->local_end (var, old);
  if (N(t) == 0) ghost (">", descend (ip, 1));
  else ghost (">", descend (descend (ip, N(t)-1), right_index (t[N(t)-1])));
  marker (descend (ip, 1));
  print (space (0, 0, env->fn->spc->max));
  penalty_min (0);
}

void
concater_rep::typeset_inactive_compound (tree t, path ip) {
  int i;
  penalty_min (0);
  marker (descend (ip, 0));
  ghost ("<", descend (descend (ip, 0), 0));
  tree old_col= env->local_begin (COLOR, "dark green");
  typeset (t[0], descend (ip, 0));
  env->local_end (COLOR, old_col);
  for (i=1; i<N(t); i++) {
    print (space (0, 0, env->fn->spc->max));
    ghost ("|", descend (descend (ip, i), 0));
    print (space (0, 0, env->fn->spc->max));
    if (i<N(t)-1) penalty_min (0);
    typeset (t[i], descend (ip, i));
  }
  ghost (">", N(t) == 0? descend (ip, 1):
	                 descend (descend (ip, i-1), right_index (t[i-1])));
  marker (descend (ip, 1));
  print (space (0, 0, env->fn->spc->max));
  penalty_min (0);
}

void
concater_rep::typeset_inactive_action (string type, tree t, path ip) {
  int i, n= N(t);
  tree old_tf;
  penalty_min (0);
  marker (descend (ip, 0));
  ghost ("<", descend (ip, 0));
  ghost (type, descend (ip, 0));
  for (i=0; i<n; i++) {
    print (space (0, 0, env->fn->spc->max));
    print (space (0, 0, env->fn->spc->max));
    ghost ("|", descend (descend (ip, i), 0));
    print (space (0, 0, env->fn->spc->max));
    if (i< (n-1)) penalty_min (0);
    if (i==(n-1)) old_tf= env->local_begin (FONT_FAMILY, "tt");
    typeset (t[i], descend (ip, i));
    if (i==(n-1)) env->local_end (FONT_FAMILY, old_tf);
    // ghost ("}", descend (descend (ip, i), right_index (t[i])));
  }
  if (N(t) == 0) ghost (">", descend (ip, 1));
  else ghost (">", descend (descend (ip, i-1), right_index (t[i-1])));
  marker (descend (ip, 1));
  print (space (0, 0, env->fn->spc->max));
  penalty_min (0);
}

void
concater_rep::typeset_inactive_string (string s, path ip) {
  penalty_min (0);
  marker (descend (ip, 0));
  ghost (s, descend (ip, 1));
  marker (descend (ip, 1));
  print (space (0, 0, env->fn->spc->max));
  penalty_min (0);
}

void
concater_rep::typeset_unknown (string which, tree t, path ip, bool flag) {
  int i;
  tree old_col;
  penalty_min (0);
  marker (descend (ip, 0));
  int start= N(a);
  ghost ("<", descend (ip, 0));
  ghost (which, descend (ip, 0), env->dis->red);
  for (i= (flag? 1: 0); i<N(t); i++) {
    print (space (0, 0, env->fn->spc->max));
    ghost ("|", descend (descend (ip, i), 0));
    print (space (0, 0, env->fn->spc->max));
    if (i<N(t)-1) penalty_min (0);
    if (i==0) old_col= env->local_begin (COLOR, "red");
    typeset (t[i], descend (ip, i));
    if (i==0) env->local_end (COLOR, old_col);
    // ghost ("}", descend (descend (ip, i), right_index (t[i])));
  }
  if (N(t) == 0) ghost (">", descend (ip, 1));
  else ghost (">", descend (descend (ip, i-1), right_index (t[i-1])));
  int end= N(a);
  for (i=start; i<end; i++)
    a[i]->b->relocate (decorate_right (ip), true);
  marker (descend (ip, 1));
  print (space (0, 0, env->fn->spc->max));
  penalty_min (0);
}

/******************************************************************************
* Dispatch
******************************************************************************/

void
concater_rep::typeset_inactive (tree t, path ip) {
  // cout << "Typeset " << t << "\n";
  if (is_atomic (t)) {
    typeset_string (t->label, ip);
    return;
  }

  switch (L (t)) {
  case UNINIT:
    typeset_uninit (t, ip);
    break;
  case ERROR:
    typeset_error (t, ip);
    break;
  case RAW_DATA:
    typeset_inactive_string ("<raw-data>", ip);
    break;
  case DOCUMENT:
    typeset_inactive_expand ("document", t, ip);
    break;
  case PARA:
    typeset_inactive_expand ("paragraph", t, ip);
    break;
  case SURROUND:
    typeset_inactive_expand ("surround", t, ip);
    break;
  case CONCAT:
    typeset_concat (t, ip);
    break;
  case GROUP:
    typeset_group (t, ip);
    break;
  case HSPACE:
    typeset_inactive_expand ("hspace", t, ip);
    break;
  case VAR_VSPACE:
    typeset_inactive_expand ("vspace*", t, ip);
    break;
  case VSPACE:
    typeset_inactive_expand ("vspace", t, ip);
    break;
  case SPACE:
    typeset_inactive_expand ("space", t, ip);
    break;
  case HTAB:
    typeset_inactive_expand ("tab", t, ip);
    break;
  case MOVE:
    typeset_inactive_expand ("move", t, ip);
    break;
  case RESIZE:
    typeset_inactive_expand ("resize", t, ip);
    break;
  case REPEAT:
    typeset_inactive_expand ("repeat", t, ip);
    break;
  case _FLOAT:
    typeset_inactive_expand ("float", t, ip);
    break;
  case DATOMS:
    typeset_inactive_expand ("decorate atoms", t, ip);
    break;
  case DLINES:
    typeset_inactive_expand ("decorate lines", t, ip);
    break;
  case DPAGES:
    typeset_inactive_expand ("decorate pages", t, ip);
    break;
  case DBOX:
    typeset_decorated_box (t, ip);
    break;

  case WITH_LIMITS:
    typeset_inactive_string ("<with limits>", ip);
    break;
  case LINE_BREAK:
    typeset_inactive_string ("<line break>", ip);
    break;
  case NEW_LINE:
  case LINE_SEP:
  case NEXT_LINE:
    {
      string name= env->drd->get_name (L(t));
      typeset_inactive_string ("<" * name * ">", ip);
      break;
    }
  case NO_BREAK:
    typeset_inactive_string ("<no-break>", ip);
    break;
  case YES_INDENT:
    typeset_inactive_string ("<do indent>", ip);
    break;
  case NO_INDENT:
    typeset_inactive_string ("<don't indent>", ip);
    break;
  case VAR_YES_INDENT:
    typeset_inactive_string ("<do indent after>", ip);
    break;
  case VAR_NO_INDENT:
    typeset_inactive_string ("<don't indent after>", ip);
    break;
  case VAR_PAGE_BREAK:
  case PAGE_BREAK:
  case VAR_NO_PAGE_BREAK:
  case NO_PAGE_BREAK:
  case VAR_NEW_PAGE:
  case NEW_PAGE:
  case VAR_NEW_DPAGE:
  case NEW_DPAGE:
    {
      string name= env->drd->get_name (L(t));
      typeset_inactive_string ("<" * name * ">", ip);
      break;
    }

  case LEFT:
    typeset_left (t, ip);
    break;
  case MID:
    typeset_middle (t, ip);
    break;
  case RIGHT:
    typeset_right (t, ip);
    break;
  case BIG:
    typeset_bigop (t, ip);
    break;
  case LPRIME:
    typeset_lprime (t, ip);
    break;
  case RPRIME:
    typeset_rprime (t, ip);
    break;
  case BELOW:
    typeset_below (t, ip);
    break;
  case ABOVE:
    typeset_above (t, ip);
    break;
  case LSUB:
  case LSUP:
    typeset_script (t, ip, false);
    break;
  case RSUB:
  case RSUP:
    typeset_script (t, ip, true);
    break;
  case FRAC:
    typeset_frac (t, ip);
    break;
  case SQRT:
    typeset_sqrt (t, ip);
    break;
  case WIDE:
    typeset_wide (t, ip, true);
    break;
  case VAR_WIDE:
    typeset_wide (t, ip, false);
    break;
  case NEG:
    typeset_neg (t, ip);
    break;
  case TREE:
    typeset_tree (t, ip);
    break;

  case TFORMAT:
    typeset_inactive_expand ("table-format", t, ip);
    break;
  case TWITH:
    typeset_inactive_expand ("table-with", t, ip);
    break;
  case CWITH:
    typeset_inactive_expand ("cell-with", t, ip);
    break;
  case TMARKER:
    typeset_inactive_expand ("table-marker", t, ip);
    break;
  case TABLE:
    typeset_table (t, ip);
    break;
  case ROW:
  case CELL:
  case SUBTABLE:
    break;

  case ASSIGN:
    typeset_inactive_expand ("assign", t, ip, 1);
    break;
  case WITH:
    typeset_inactive_expand ("with", t, ip, N(t)-1);
    break;
  case PROVIDES:
    typeset_inactive_expand ("provides", t, ip);
    break;
  case VALUE:
    typeset_inactive_expand ("value", t, ip);
    break;
  case MACRO:
    typeset_inactive_expand ("macro", t, ip, N(t)-1);
    break;
  case DRD_PROPS:
    typeset_inactive_expand ("drd-properties", t, ip, 1);
    break;
  case ARG:
    typeset_inactive_expand ("argument", t, ip);
    break;
  case COMPOUND:
    typeset_inactive_compound (t, ip);
    break;
  case XMACRO:
    typeset_inactive_expand ("xmacro", t, ip, 1);
    break;
  case GET_LABEL:
    typeset_inactive_expand ("tree-label", t, ip);
    break;
  case GET_ARITY:
    typeset_inactive_expand ("arity", t, ip);
    break;
  case MAP_ARGS:
    typeset_inactive_expand ("map-args", t, ip);
    break;
  case EVAL_ARGS:
    typeset_inactive_expand ("eval-args", t, ip);
    break;
  case EVAL:
    typeset_inactive_expand ("eval", t, ip);
    break;
  case QUOTE:
    typeset_inactive_expand ("quote", t, ip);
    break;
  case DELAY:
    typeset_inactive_expand ("delay", t, ip);
    break;
  case HOLD:
    typeset_inactive_expand ("hold", t, ip);
    break;
  case RELEASE:
    typeset_inactive_expand ("release", t, ip);
    break;
  case EXTERN:
    typeset_inactive_expand ("extern", t, ip);
    break;
  case INCLUDE:
    typeset_inactive_expand ("include", t, ip);
    break;

  case OR:
    typeset_inactive_expand ("or", t, ip);
    break;
  case XOR:
    typeset_inactive_expand ("xor", t, ip);
    break;
  case AND:
    typeset_inactive_expand ("and", t, ip);
    break;
  case NOT:
    typeset_inactive_expand ("not", t, ip);
    break;
  case PLUS:
    typeset_inactive_expand ("+", t, ip);
    break;
  case MINUS:
    typeset_inactive_expand ("-", t, ip);
    break;
  case TIMES:
    typeset_inactive_expand ("*", t, ip);
    break;
  case OVER:
    typeset_inactive_expand ("/", t, ip);
    break;
  case DIV:
    typeset_inactive_expand ("div", t, ip);
    break;
  case MOD:
    typeset_inactive_expand ("mod", t, ip);
    break;
  case MERGE:
    typeset_inactive_expand ("merge", t, ip);
    break;
  case LENGTH:
    typeset_inactive_expand ("length", t, ip);
    break;
  case RANGE:
    typeset_inactive_expand ("range", t, ip);
    break;
  case NUMBER:
    typeset_inactive_expand ("number", t, ip);
    break;
  case _DATE:
    typeset_inactive_expand ("date", t, ip);
    break;
  case TRANSLATE:
    typeset_inactive_expand ("translate", t, ip);
    break;
  case FIND_FILE:
    typeset_inactive_expand ("find file", t, ip);
    break;
  case IS_TUPLE:
    typeset_inactive_expand ("is tuple", t, ip);
    break;
  case LOOK_UP:
    typeset_inactive_expand ("look up", t, ip);
    break;
  case EQUAL:
    typeset_inactive_expand ("equal", t, ip);
    break;
  case UNEQUAL:
    typeset_inactive_expand ("unequal", t, ip);
    break;
  case LESS:
    typeset_inactive_expand ("less", t, ip);
    break;
  case LESSEQ:
    typeset_inactive_expand ("less or equal", t, ip);
    break;
  case GREATER:
    typeset_inactive_expand ("greater", t, ip);
    break;
  case GREATEREQ:
    typeset_inactive_expand ("greater or equal", t, ip);
    break;
  case IF:
    typeset_inactive_expand ("if", t, ip);
    break;
  case VAR_IF:
    typeset_inactive_expand ("if*", t, ip);
    break;
  case CASE:
    typeset_inactive_expand ("case", t, ip);
    break;
  case WHILE:
    typeset_inactive_expand ("while", t, ip);
    break;

  case INACTIVE:
    typeset_inactive_tag (t, ip);
    break;
  case ACTIVE:
    typeset (t[0], descend (ip, 0));
    break;
  case VAR_INACTIVE:
    typeset_inactive_tag (t, ip);
    break;
  case VAR_ACTIVE:
    typeset (t[0], descend (ip, 0));
    break;
  case SYMBOL:
    typeset_inactive_symbol (t, ip);
    break;
  case LATEX:
    typeset_inactive_latex (t, ip);
    break;
  case HYBRID:
    typeset_inactive_hybrid (t, ip);
    break;
  case TUPLE:
    typeset_inactive_expand ("tuple", t, ip);
    break;
  case ATTR:
    typeset_inactive_expand ("attr", t, ip);
    break;
  case COLLECTION:
    typeset_inactive_expand ("collection", t, ip);
    break;
  case ASSOCIATE:
    typeset_inactive_expand ("associate", t, ip);
    break;
  case BACKUP:
    typeset_inactive_expand ("backup", t, ip);
    break;
  case LABEL:
    typeset_inactive_expand ("label", t, ip);
    break;
  case REFERENCE:
    typeset_inactive_expand ("reference", t, ip);
    break;
  case PAGEREF:
    typeset_inactive_expand ("page reference", t, ip);
    break;
  case WRITE:
    typeset_inactive_expand ("write", t, ip);
    break;
  case SPECIFIC:
    typeset_inactive_specific (t, ip);
    break;
  case HLINK:
    typeset_inactive_action ("hyperlink", t, ip);
    break;
  case ACTION:
    typeset_inactive_action ("action", t, ip);
    break;
  case TAG:
    typeset_inactive_expand ("tag", t, ip);
    break;
  case MEANING:
    typeset_inactive_expand ("meaning", t, ip);
    break;
  case FLAG:
    typeset_inactive_expand ("flag", t, ip);
    break;

  case GRAPHICS:
    typeset_inactive_expand ("graphics", t, ip);
    break;
  case SUPERPOSE:
    typeset_inactive_expand ("superpose", t, ip);
    break;
  case TEXT_AT:
    typeset_inactive_expand ("text at", t, ip);
    break;
  case _POINT:
    typeset_inactive_expand ("point", t, ip);
    break;
  case LINE:
    typeset_inactive_expand ("line", t, ip);
    break;
  case CLINE:
    typeset_inactive_expand ("cline", t, ip);
    break;
  case SPLINE:
    typeset_inactive_expand ("spline", t, ip);
    break;
  case VAR_SPLINE:
    typeset_inactive_expand ("var_spline", t, ip);
    break;
  case CSPLINE:
    typeset_inactive_expand ("cspline", t, ip);
    break;
  case FILL:
    typeset_inactive_expand ("fill", t, ip);
    break;
  case POSTSCRIPT:
    typeset_inactive_expand ("postscript", t, ip);
    break;
  default:
    typeset_inactive_expand (as_string (L(t)), t, ip);
    break;
  }
}
