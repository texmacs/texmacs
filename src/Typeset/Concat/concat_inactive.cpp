
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
* Syntactic coloring of arguments of inactive markup
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
concater_rep::typeset_inactive_arg (tree t, path ip, int i) {
  // NOTE: the branching might be done on base of the DRD rather than L(t).
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

/******************************************************************************
* Display disactivated markup using angular brackets
******************************************************************************/

void
concater_rep::typeset_inactive_angular (tree t, path ip, bool err) {
  int i, first= 1;
  string name= as_string (L(t));
  marker (descend (ip, 0));
  int start= N(a);
  ghost ("<", descend (ip, 0));
  switch (L(t)) {
  case RAW_DATA:
    ghost (name, descend (ip, 1));
    break;
  case COMPOUND:
    if (err) typeset_colored (t[0], descend (ip, 0), "red");
    else typeset_colored (t[0], descend (ip, 0), "dark green");
    break;
  case SYMBOL:
    typeset_colored (t[0], descend (ip, 0), "blue");
    break;
  case HYBRID:
    ghost ("\\", descend (ip, 0));
    typeset_colored (t[0], descend (ip, 0), "dark green");
    break;
  default:
    if (err) ghost (name, descend (ip, 0), env->dis->red);
    else ghost (name, descend (ip, 0));
    first= 0;
    break;
  }

  for (i= first; i<N(t); i++) {
    print (space (0, 0, env->fn->spc->max));
    ghost ("|", descend (descend (ip, i), 0));
    /*if (i<N(t)-1)*/ penalty_min (0);
    print (space (0, 0, env->fn->spc->max));
    typeset_inactive_arg (t, ip, i);
  }

  if (i==first) ghost (">", descend (ip, 1));
  else ghost (">", descend (descend (ip, i-1), right_index (t[i-1])));
  if (err) {
    int end= N(a);
    for (i=start; i<end; i++)
      a[i]->b->relocate (decorate_right (ip), true);
  }
  marker (descend (ip, 1));
}

/******************************************************************************
* Dispatching
******************************************************************************/

void
concater_rep::typeset_inactive (tree t, path ip) {
  if (is_atomic (t)) {
    typeset_string (t->label, ip);
    return;
  }
  switch (L (t)) {
  case CONCAT:
    typeset_concat (t, ip);
    break;
  case ACTIVE:
    typeset (t[0], descend (ip, 0));
    break;
  case VAR_ACTIVE:
    typeset (t[0], descend (ip, 0));
    break;
  default:
    typeset_inactive_angular (t, ip, false);
    break;
  }
}

void
concater_rep::typeset_error (tree t, path ip) {
  if (is_atomic (t)) typeset_string (t->label, ip);
  else typeset_inactive_angular (t, ip, true);
}

/******************************************************************************
* Display disactivated markup using angular brackets
******************************************************************************/

void
concater_rep::typeset_src_open (tree t, path ip, string extra) {
  bool visual_open=
    // Visually speaking, the tag is an opening tag
    (L(t) == INLINE_TAG) || (L(t) == OPEN_TAG) ||
    (env->src_close == CLOSE_REPEAT);
  bool visual_close=
    // Visually speaking, the tag is a closing tag
    (L(t) == INLINE_TAG) || (L(t) == CLOSE_TAG) ||
    (env->src_close == CLOSE_REPEAT);

  if (visual_open)
    if ((L(t) != INLINE_TAG) && (env->src_close == CLOSE_MINIMAL)) {
      typeset_colored (t[0], descend (ip, 0), "blue");
      if (N(t) > 1) {
	penalty_min (0);
	print (env->fn->spc);
      }
      return;
    }

  if (visual_open) {
    path dip= descend (ip, 0);
    path nip= descend (descend (ip, 0), right_index (t[0]));
    switch (env->src_style) {
    case STYLE_ANGULAR:
      ghost ("<", dip);
      ghost (extra, dip);
      typeset_colored (t[0], descend (ip, 0), "blue");
      break;
    case STYLE_SCHEME:
      ghost ("(", dip);
      ghost (extra, dip);
      typeset_colored (t[0], descend (ip, 0), "blue");
      break;
    case STYLE_LATEX:
      //ghost ("\\", dip);
      //if (extra == "\\") ghost ("begin", dip);
      //else if (extra == "|") ghost ("continue", dip);
      //else if (extra == "/") ghost ("end", dip);
      //if (extra != "") ghost ("{", dip);
      ghost (extra, dip);
      typeset_colored (t[0], descend (ip, 0), "blue");
      //if (extra != "") ghost ("}", nip);
      break;
    case STYLE_FUNCTIONAL:
      ghost (extra, dip);
      typeset_colored (t[0], descend (ip, 0), "blue");
      break;
    }
  }

  if (visual_close && (N(t) == 1))
    // No arguments or block arguments follow
    return;

  path dip = descend (descend (ip, 1), 0);
  if (N(t) == 1) dip= descend (descend (ip, 0), right_index (t[0]));
  if (visual_open) {
    if (env->src_style == STYLE_LATEX) ghost ("{", dip);
    else if (env->src_style == STYLE_FUNCTIONAL) {
      print (env->fn->spc / 2);
      ghost ("(", dip);
    }
    else typeset_src_middle (t, ip, 1);
  }
  else typeset_src_middle (t, ip, 1);
}

void
concater_rep::typeset_src_middle (tree t, path ip, int i) {
  path pip= descend (descend (ip, i-1), right_index (t[i-1]));
  path dip= descend (descend (ip, i), 0);
  if (i == N(t)) dip= pip;
  switch (env->src_style) {
  case STYLE_ANGULAR:
    print (space (0, 0, env->fn->spc->max));
    ghost ("|", dip);
    penalty_min (0);
    print (space (0, 0, env->fn->spc->max));
    break;
  case STYLE_SCHEME:
    penalty_min (0);
    print (env->fn->spc);
    break;
  case STYLE_LATEX:
    if ((L(t) != INLINE_TAG) && (env->src_close == CLOSE_MINIMAL)) {
      ghost (",", dip);
      penalty_min (0);
      print (env->fn->spc / 2);
    }
    else {
      ghost ("}", pip);
      penalty_min (0);
      ghost ("{", dip);
    }
    break;
  case STYLE_FUNCTIONAL:
    ghost (",", dip);
    penalty_min (0);
    print (env->fn->spc / 2);
    break;
  }
}

void
concater_rep::typeset_src_close (tree t, path ip) {
  path dip= descend (descend (ip, N(t)-1), right_index (t[N(t)-1]));
  switch (env->src_style) {
  case STYLE_ANGULAR:
    ghost (">", dip);
    break;
  case STYLE_SCHEME:
    ghost (")", dip);
    break;
  case STYLE_LATEX:
    ghost ("}", dip);
    break;
  case STYLE_FUNCTIONAL:
    ghost (")", dip);
    break;
  }
}

void
concater_rep::typeset_src_args (tree t, path ip) {
  int i, n= N(t);
  for (i=1; i<n; i++) {
    if (i>1) typeset_src_middle (t, ip, i);
    typeset (t[i], descend (ip, i));
  }
}

void
concater_rep::typeset_src_tag (tree t, path ip) {
  int n= N(t);
  marker (descend (ip, 0));
  if ((L(t) == INLINE_TAG) || (env->src_close == CLOSE_REPEAT)) {
    string extra;
    if (L(t) == OPEN_TAG) extra= "\\";
    else if (L(t) == MIDDLE_TAG) extra= "|";
    else if (L(t) == CLOSE_TAG) extra= "/";
    typeset_src_open (t, ip, extra);
    typeset_src_args (t, ip);
    if ((n>1) ||
	(env->src_style == STYLE_ANGULAR) || (env->src_style == STYLE_SCHEME))
      typeset_src_close (t, ip);
  }
  else {
    typeset_src_open (t, ip, "");
    typeset_src_args (t, ip);
    if ((n>1) || (L(t) == CLOSE_TAG))
      if ((L(t) == MIDDLE_TAG) || (env->src_close != CLOSE_MINIMAL)) {
	if (L(t) == CLOSE_TAG) typeset_src_close (t, ip);
	else typeset_src_middle (t, ip, n);
      }
  }
  marker (descend (ip, 1));
}
