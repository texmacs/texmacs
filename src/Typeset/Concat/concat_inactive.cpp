
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
concater_rep::typeset_eval_args (tree t, path ip) { 
  marker (descend (ip, 0));
  typeset (env->exec (t), decorate_right (ip), false);
  marker (descend (ip, 1));
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
concater_rep::typeset_colored (tree t, path ip, tree val, bool test) {
  if (test) {
    tree old_val= env->local_begin (COLOR, val);
    typeset (t, ip);
    env->local_end (COLOR, old_val);
  }
  else typeset (t, ip);
}

void
concater_rep::typeset_tt (tree t, path ip, bool test) {
  if (test) {
    tree val;
    string var, mode= env->get_string (MODE);
    if (mode == "text") { var=FONT_FAMILY; val="tt"; }
    else if (mode == "math") { var=MATH_FONT_FAMILY; val="mt"; }
    else { var=PROG_FONT_FAMILY; val="tt"; }
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
    typeset_colored (t[i], descend (ip, i), "dark green", i==0);
    break;
  case WITH:
    typeset_colored (t[i], descend (ip, i), "dark green",
		     (i<n-1) && ((i&1)==0));
    break;
  case TWITH:
  case CWITH:
    typeset_colored (t[i], descend (ip, i), "dark green", i==n-2);
    break;
  case MACRO:
    typeset_colored (t[i], descend (ip, i), "brown", i<n-1);
    break;
  case XMACRO:
  case ARG:
    typeset_colored (t[i], descend (ip, i), "brown", i==0);
    break;
  case MAP_ARGS:
    if (i<2) typeset_colored (t[i], descend (ip, i), "dark green");
    else typeset_colored (t[i], descend (ip, i), "brown", i==2);
    break;
  case SPECIFIC:
    typeset_tt (t[i], descend (ip, i),
		(i==1) && (t[0] != "texmacs") &&
		(t[0] != "screen") && (t[0] != "printer"));
    break;
  case ACTION:
    typeset_tt (t[i], descend (ip, i), i==n-1);
    break;
  default:
    typeset (t[i], descend (ip, i));
    break;
  }
}

void
concater_rep::typeset_inactive_angular (tree t, path ip) {
  int i, start= 1;
  string name= as_string (L(t));
  //string name= replace (env->drd->get_name (L(t)), " ", "-");

  penalty_min (0);
  marker (descend (ip, 0));
  ghost ("<", descend (ip, 0));
  switch (L(t)) {
  case RAW_DATA:
    ghost (name, descend (ip, 1));
    break;
  case COMPOUND:
    typeset_colored (t[0], descend (ip, 0), "dark green");
    break;
  case SYMBOL:
    typeset_colored (t[0], descend (ip, 0), "blue");
    break;
  case HYBRID:
    ghost ("\\", descend (ip, 0));
    typeset_colored (t[0], descend (ip, 0), "dark green");
    break;
  default:
    ghost (name, descend (ip, 0));
    start= 0;
    break;
  }

  for (i= start; i<N(t); i++) {
    print (space (0, 0, env->fn->spc->max));
    print (space (0, 0, env->fn->spc->max));
    ghost ("|", descend (descend (ip, i), 0));
    print (space (0, 0, env->fn->spc->max));
    if (i<N(t)-1) penalty_min (0);
    typeset_inactive_angular_arg (t, ip, i);
  }

  if (i==start) ghost (">", descend (ip, 1));
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
  case CONCAT:
    typeset_concat (t, ip);
    break;

    /*
  case TABLE:
    typeset_table (t, ip);
    break;
    */

  case ACTIVE:
    typeset (t[0], descend (ip, 0));
    break;
  case VAR_ACTIVE:
    typeset (t[0], descend (ip, 0));
    break;

  default:
    typeset_inactive_angular (t, ip);
    break;
  }
}
