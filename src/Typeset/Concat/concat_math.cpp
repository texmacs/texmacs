
/******************************************************************************
* MODULE     : concat_math.cpp
* DESCRIPTION: Typesetting mathematical markup
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "concater.hpp"
#include "analyze.hpp"

/******************************************************************************
* Typesetting special mathematical symbols
******************************************************************************/

void
concater_rep::typeset_large (tree t, path ip, int type, string prefix) {
  if ((N(t) == 1) && is_atomic (t[0])) {
    string s= prefix * t[0]->label * ">";
    box b= text_box (ip, 0, s, env->fn, env->col);
    print (type, b);
    // temporarary: use parameters from group-open class in std-math.syx
    // bug: allow hyphenation after ) and before *
  }
  else if ((N(t) == 2) && is_atomic (t[0]) && is_int (t[1])) {
    string s= prefix * t[0]->label * "-" * t[1]->label * ">";
    box b= text_box (ip, 0, s, env->fn, env->col);
    SI dy= env->fn->yfrac - ((b->y1 + b->y2) >> 1);
    box mvb= move_box (ip, b, 0, dy, false, true);
    print (STD_ITEM, macro_box (ip, mvb, env->fn));
  }
  else if ((N(t) >= 2) && is_atomic (t[0])) {
    SI y1, y2;
    if (N(t) == 2) {
      SI l= env->as_length (t[1]) >> 1;
      y1= env->fn->yfrac - l;
      y2= env->fn->yfrac + l;
    }
    else {
      y1= env->as_length (t[1]);
      y2= env->as_length (t[2]);
    }
    string s= prefix * t[0]->label * ">";
    box b= delimiter_box (ip, s, env->fn, env->col, y1, y2);
    print (STD_ITEM, b);
  }
}

void
concater_rep::typeset_bigop (tree t, path ip) {
  if ((N(t) == 1) && is_atomic (t[0])) {
    space spc= env->fn->spc;
    string l= t[0]->label;
    string s= "<big-" * l * ">";
    bool flag= (!env->math_condensed) && (l != ".");
    box b= big_operator_box (ip, s, env->fn, env->col,
			     env->display_style? 2: 1);
    if (flag) print (space (spc->min>>1, spc->def>>1, spc->max));
    print (STD_ITEM, b);
    penalty_min (HYPH_PANIC);
    if ((l != "int") && (l != "oint")) with_limits (LIMITS_DISPLAY);
    if (flag) print (spc);
    // temporarary: use parameters from operator-big class in std-math.syx
  }
}

void
concater_rep::typeset_lprime (tree t, path ip) {
  if ((N(t) == 1) && is_atomic (t[0])) {
    tree old_il= env->local_begin_script ();
    path sip= descend (ip, 0);
    box b1, b2= typeset_as_concat (env, t[0], sip);
    b2= symbol_box (sip, b2, N(t[0]->label));
    b2= move_box (sip, b2, env->as_length (string ("-0.05fn")), 0);
    env->local_end_script (old_il);
    print (LSUP_ITEM, script_box (ip, b1, b2, env->fn));
    penalty_max (HYPH_INVALID);
  }
}

void
concater_rep::typeset_rprime (tree t, path ip) {
  if ((N(t) == 1) && is_atomic (t[0])) {
    tree old_il= env->local_begin_script ();
    path sip= descend (ip, 0);
    box b1, b2= typeset_as_concat (env, t[0], sip);
    b2= symbol_box (sip, b2, N(t[0]->label));
    b2= move_box (sip, b2, env->as_length (string ("0.05fn")), 0);
    env->local_end_script (old_il);
    penalty_max (HYPH_INVALID);
    if (N(a)>0) a[N(a)-1]->limits= false;
    print (RSUP_ITEM, script_box (ip, b1, b2, env->fn));
  }
}

/******************************************************************************
* Typesetting scripts
******************************************************************************/

void
concater_rep::typeset_below (tree t, path ip) {
  box b1= typeset_as_concat (env, t[0], descend (ip, 0));
  tree old_ds= env->local_begin (MATH_DISPLAY, "false");
  tree old_mc= env->local_begin (MATH_CONDENSED, "true");
  tree old_il= env->local_begin_script ();
  box b2= typeset_as_concat (env, t[1], descend (ip, 1));
  env->local_end_script (old_il);
  env->local_end (MATH_CONDENSED, old_mc);
  env->local_end (MATH_DISPLAY, old_ds);
  print (STD_ITEM, limit_box (ip, b1, b2, box (), env->fn, false));
}

void
concater_rep::typeset_above (tree t, path ip) {
  box b1= typeset_as_concat (env, t[0], descend (ip, 0));
  tree old_ds= env->local_begin (MATH_DISPLAY, "false");
  tree old_mc= env->local_begin (MATH_CONDENSED, "true");
  tree old_il= env->local_begin_script ();
  box b2= typeset_as_concat (env, t[1], descend (ip, 1));
  env->local_end_script (old_il);
  env->local_end (MATH_CONDENSED, old_mc);
  env->local_end (MATH_DISPLAY, old_ds);
  print (STD_ITEM, limit_box (ip, b1, box (), b2, env->fn, false));
}

void
concater_rep::typeset_script (tree t, path ip, bool right) {
  int type= RSUP_ITEM;
  box b1, b2;
  tree old_ds= env->local_begin (MATH_DISPLAY, "false");
  tree old_mc= env->local_begin (MATH_CONDENSED, "true");
  tree old_il= env->local_begin_script ();
  if (is_func (t, SUB (right))) {
    tree old_vp= env->local_begin (MATH_VPOS, "-1");
    b1= typeset_as_concat (env, t[0], descend (ip, 0));
    type= right? RSUB_ITEM: LSUB_ITEM;
    env->local_end (MATH_VPOS, old_vp);
  }
  if (is_func (t, SUP (right))) {
    tree old_vp= env->local_begin (MATH_VPOS, "1");
    b2= typeset_as_concat (env, t[0], descend (ip, 0));
    type= right? RSUP_ITEM: LSUP_ITEM;
    env->local_end (MATH_VPOS, old_vp);
  }
  env->local_end_script (old_il);
  env->local_end (MATH_CONDENSED, old_mc);
  env->local_end (MATH_DISPLAY, old_ds);
  if (right) penalty_max (HYPH_INVALID);
  a << line_item (type, script_box (ip, b1, b2, env->fn), HYPH_INVALID);
  // do not use print, because of italic space
  if (!right) penalty_max (HYPH_INVALID);
}

/******************************************************************************
* Standard mathematical operations
******************************************************************************/

void
concater_rep::typeset_frac (tree t, path ip) {
  bool disp= env->display_style;
  tree old;
  if (disp) old= env->local_begin (MATH_DISPLAY, "false");
  else old= env->local_begin_script ();
  tree old_vp= env->local_begin (MATH_VPOS, "1");
  box nom= typeset_as_concat (env, t[0], descend (ip, 0));
  env->local_end (MATH_VPOS, "-1");
  box den= typeset_as_concat (env, t[1], descend (ip, 1));
  env->local_end (MATH_VPOS, old_vp);
  font sfn= env->fn;
  if (disp) env->local_end (MATH_DISPLAY, old);
  else env->local_end_script (old);
  print (STD_ITEM, frac_box (ip, nom, den, env->fn, sfn, env->col));
}

void
concater_rep::typeset_sqrt (tree t, path ip) {
  if (N(t)==0) return; // FIXME: this should never happen
  box b= typeset_as_concat (env, t[0], descend (ip, 0));
  box ind;
  if (N(t)==2) {
    bool disp= env->display_style;
    tree old;
    if (disp) old= env->local_begin (MATH_DISPLAY, "false");
    tree old_il= env->local_begin_script ();
    ind= typeset_as_concat (env, t[1], descend (ip, 1));
    env->local_end_script (old_il);
    if (disp) env->local_end (MATH_DISPLAY, old);
  }
  SI sep= env->fn->sep;
  box sqrtb= delimiter_box (decorate_left (ip), "<large-sqrt>",
			    env->fn, env->col, b->y1, b->y2+ sep);
  print (STD_ITEM, sqrt_box (ip, b, ind, sqrtb, env->fn, env->col));
}

void
concater_rep::typeset_wide (tree t, path ip, bool above) {
  bool wide;
  box b= typeset_as_concat (env, t[0], descend (ip, 0));
  string s= as_string (t[1]);
  if (env->get_string (MATH_FONT) == "adobe") {
    if (s == "^") s= "<hat>";
    if (s == "~") s= "<tilde>";
  }
  if (starts (s, "<wide-")) {
    s= "<" * s (6, N(s));
    wide= true;
  }
  else {
    wide= (b->w() >= (env->fn->wfn));
    if (ends (s, "dot>") || (s == "<acute>") ||
	(s == "<grave>") || (s == "<abovering>"))
      wide= false;
  }
  if (wide) {
    SI w  = env->fn->wline;
    box wideb;
    if ((s == "^") || (s == "<hat>"))
      wideb= wide_hat_box   (decorate_middle (ip), b->x1, b->x2, w, env->col);
    else if ((s == "~") || (s == "<tilde>"))
      wideb= wide_tilda_box (decorate_middle (ip), b->x1, b->x2, w, env->col);
    else if (s == "<bar>")
      wideb= wide_bar_box   (decorate_middle (ip), b->x1, b->x2, w, env->col);
    else if (s == "<vect>")
      wideb= wide_vect_box  (decorate_middle (ip), b->x1, b->x2, w, env->col);
    else if (s == "<check>")
      wideb= wide_check_box (decorate_middle (ip), b->x1, b->x2, w, env->col);
    else if (s == "<breve>")
      wideb= wide_breve_box (decorate_middle (ip), b->x1, b->x2, w, env->col);
    else if (s == "<squnderbrace>" || s == "<squnderbrace*>")
      wideb= wide_squbr_box (decorate_middle (ip), b->x1, b->x2, w, env->col);
    else if (s == "<sqoverbrace>" || s == "<sqoverbrace*>")
      wideb= wide_sqobr_box (decorate_middle (ip), b->x1, b->x2, w, env->col);
    else wideb= wide_box (decorate_middle (ip),
			  "<rubber-" * s (1, N(s)-1) * ">",
			  env->fn, env->col, b->x2- b->x1);
    print (STD_ITEM, wide_box (ip, b, wideb, env->fn, env->fn->sep, above));
    if ((s == "<underbrace>") || (s == "<overbrace>") ||
	(s == "<squnderbrace>") || (s == "<sqoverbrace>"))
      with_limits (LIMITS_ALWAYS);
  }
  else {
    SI sep= above? -env->fn->yx: env->fn->sep;
    box wideb= text_box (decorate_middle (ip), 0, s, env->fn, env->col);
    print (STD_ITEM, wide_box (ip, b, wideb, env->fn, sep, above));
  }
}

void
concater_rep::typeset_neg (tree t, path ip) {
  box b= typeset_as_concat (env, t[0], descend (ip, 0));
  print (STD_ITEM, neg_box (ip, b, env->fn, env->col));
}

/******************************************************************************
* Other markup
******************************************************************************/

void
concater_rep::typeset_group (tree t, path ip) {
  marker (descend (ip, 0));
  typeset (t[0], descend (ip, 0));
  marker (descend (ip, 1));
}

static string
bracket_color (int nl) {
  switch (nl % 3) {
  case 0 : return "#662266";
  case 1 : return "#226666";
  default: return "#663322";
  }
}

void
concater_rep::typeset_around (tree t, path ip) {
  int nl= env->get_int (MATH_NESTING_LEVEL);
  tree old_col= env->local_begin (COLOR, bracket_color (nl));
  tree old_nl = env->local_begin (MATH_NESTING_LEVEL, as_string (nl+1));
  marker (descend (ip, 0));
  typeset (t[0], descend (ip, 0));
  typeset (t[1], descend (ip, 1));
  typeset (t[2], descend (ip, 2));
  marker (descend (ip, 1));
  env->local_end (MATH_NESTING_LEVEL, old_nl);
  env->local_end (COLOR, old_col);
}

void
concater_rep::typeset_tree (tree t, path ip) {
  int i, n= N(t);
  array<box> bs(n);
  for (i=0; i<n; i++) bs[i]= typeset_as_concat (env, t[i], descend (ip, i));
  print (STD_ITEM, tree_box (ip, bs, env->fn, env->col));
}

void
concater_rep::typeset_table (tree t, path ip) {
  box b= typeset_as_table (env, t, ip);
  print (STD_ITEM, b);
}
