
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
#include "analyze.hpp"

/******************************************************************************
* Typesetting special deactivated trees
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
* Angular deactivated representation
******************************************************************************/

void
concater_rep::typeset_modified (
  tree t, path ip, string var, tree val, bool test)
{
  if (test) {
    tree old_val= env->local_begin (var, val);
    typeset (t, ip);
    env->local_end (var, old_val);
  }
  else typeset (t, ip);
}

void
concater_rep::typeset_inactive_angular_arg (tree t, path ip, int i) {
  // NOTE: the branching information might also be stored in the DRD.
  int n= N(t);
  switch (L(t)) {
  case ASSIGN:
  case DRD_PROPS:
  case VALUE:
    typeset_modified (t[i], descend (ip, i), COLOR, "dark green", i==0);
    break;
  case WITH:
    typeset_modified (t[i], descend (ip, i), COLOR, "dark green",
		      (i<n-1) && ((i&1)==0));
    break;
  case TWITH:
  case CWITH:
    typeset_modified (t[i], descend (ip, i), COLOR, "dark green", i==n-2);
    break;
  case MACRO:
    typeset_modified (t[i], descend (ip, i), COLOR, "brown", i<n-1);
    break;
  case XMACRO:
  case ARG:
    typeset_modified (t[i], descend (ip, i), COLOR, "brown", i==0);
    break;
  case MAP_ARGS:
    if (i<2) typeset_modified (t[i], descend (ip, i), COLOR, "dark green");
    else typeset_modified (t[i], descend (ip, i), COLOR, "brown", i==2);
    break;
  case ACTION:
    typeset_modified (t[i], descend (ip, i), FONT_FAMILY, "tt", i==n-1);
    break;
  default:
    typeset (t[i], descend (ip, i));
    break;
  }
}

void
concater_rep::typeset_inactive_angular (tree t, path ip) {
  int i=0;
  string type= as_string (L(t));
  //string type= replace (env->drd->get_name (L(t)), " ", "-");

  penalty_min (0);
  marker (descend (ip, 0));
  ghost ("<", descend (ip, 0));
  switch (L(t)) {
  case HYBRID:
    ghost ("\\", descend (ip, 0));
    typeset_modified (t[0], descend (ip, 0), COLOR, "dark green");
    i=1;
    break;
  default:
    ghost (type, descend (ip, 0));
    break;
  }

  for (; i<N(t); i++) {
    print (space (0, 0, env->fn->spc->max));
    print (space (0, 0, env->fn->spc->max));
    ghost ("|", descend (descend (ip, i), 0));
    print (space (0, 0, env->fn->spc->max));
    if (i<N(t)-1) penalty_min (0);
    typeset_inactive_angular_arg (t, ip, i);
  }

  if (i==0) ghost (">", descend (ip, 1));
  else ghost (">", descend (descend (ip, i-1), right_index (t[i-1])));
  marker (descend (ip, 1));
  print (space (0, 0, env->fn->spc->max));
  penalty_min (0);
}

/******************************************************************************
* Dispatching
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
  case CONCAT:
    typeset_concat (t, ip);
    break;

    /*
  case TABLE:
    typeset_table (t, ip);
    break;
    */

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
  case SPECIFIC:
    typeset_inactive_specific (t, ip);
    break;

  default:
    typeset_inactive_angular (t, ip);
    break;
  }
}
