
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
concater_rep::typeset_inactive (tree t, path ip) { 
  marker (descend (ip, 0));
  typeset (t[0], descend (ip, 0), false);
  marker (descend (ip, 1));
}

void
concater_rep::typeset_inactive (
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
  ghost (">", descend (descend (ip, N(t)-1), right_index (t[N(t)-1])));
  // ghost (">", descend (ip, 1));
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
    if (mode == "text") { var=TEXT_FAMILY; value="tt"; }
    else if (mode == "math") { var=MATH_FAMILY; value="mt"; }
    else { var=PROG_FAMILY; value="tt"; }
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
  ghost (">", descend (descend (ip, N(t)-1), right_index (t[N(t)-1])));
  marker (descend (ip, 1));
  print (space (0, 0, env->fn->spc->max));
  penalty_min (0);
}

void
concater_rep::typeset_inactive_expand_apply (tree t, path ip, bool flag) {
  int i;
  penalty_min (0);
  marker (descend (ip, 0));
  ghost (flag? string ("<"): string ("{"),
	 descend (descend (ip, 0), 0));
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
  ghost (flag? string (">"): string ("}"),
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
    if (i==(n-1)) old_tf= env->local_begin (TEXT_FAMILY, "tt");
    typeset (t[i], descend (ip, i));
    if (i==(n-1)) env->local_end (TEXT_FAMILY, old_tf);
    // ghost ("}", descend (descend (ip, i), right_index (t[i])));
  }
  ghost (">", descend (descend (ip, i-1), right_index (t[i-1])));
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
  ghost (">", descend (descend (ip, i-1), right_index (t[i-1])));
  int end= N(a);
  for (i=start; i<end; i++)
    a[i]->b->relocate (decorate_right (ip), true);
  marker (descend (ip, 1));
  print (space (0, 0, env->fn->spc->max));
  penalty_min (0);
}
