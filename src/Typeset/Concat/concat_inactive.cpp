
/******************************************************************************
* MODULE     : concat_inactive.cpp
* DESCRIPTION: Typesetting of inactive markup
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "concater.hpp"
#include "analyze.hpp"

/******************************************************************************
* Subroutine for syntactic coloring
******************************************************************************/

void
concater_rep::typeset_blue (tree t, path ip) {
  tree old_mode= env->local_begin (MODE, "src");
  tree old_col = env->local_begin (COLOR, env->src_tag_color);
  tree old_fam = env->local_begin (FONT_FAMILY, "ss");
  typeset (t, ip);
  env->local_end (FONT_FAMILY, old_fam);
  env->local_end (COLOR, old_col);
  env->local_end (MODE, old_mode);
}

/******************************************************************************
* Rendering of source tree tags
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
      typeset_blue (t[0], descend (ip, 0));
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
      typeset_blue (t[0], descend (ip, 0));
      break;
    case STYLE_SCHEME:
      ghost ("(", dip);
      ghost (extra, dip);
      typeset_blue (t[0], descend (ip, 0));
      break;
    case STYLE_LATEX:
      //ghost ("\\", dip);
      //if (extra == "\\") ghost ("begin", dip);
      //else if (extra == "|") ghost ("continue", dip);
      //else if (extra == "/") ghost ("end", dip);
      //if (extra != "") ghost ("{", dip);
      ghost (extra, dip);
      typeset_blue (t[0], descend (ip, 0));
      //if (extra != "") ghost ("}", nip);
      break;
    case STYLE_FUNCTIONAL:
      ghost (extra, dip);
      typeset_blue (t[0], descend (ip, 0));
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
  if (N(t) == 0) { typeset_error (t, ip); return; }
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

/******************************************************************************
* Inactive and erroneous markup
******************************************************************************/

void
concater_rep::typeset_inactive (tree t, path ip) {
  tree m (MACRO, "x", tree (REWRITE_INACTIVE, tree (ARG, "x"), "once"));
  typeset_auto (t, ip, m);
}

void
concater_rep::typeset_error (tree t, path ip) {
  tree m (MACRO, "x", tree (REWRITE_INACTIVE, tree (ARG, "x"), "error"));
  marker (descend (ip, 0));
  typeset_auto (attach_right (t, ip), m);
  marker (descend (ip, 1));
}
