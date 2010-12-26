
/******************************************************************************
* MODULE     : concat_text.cpp
* DESCRIPTION: Typesetting textual markup
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "concater.hpp"
#include "formatter.hpp"
#include "analyze.hpp"

lazy make_lazy_vstream (edit_env env, tree t, path ip, tree channel);

/******************************************************************************
* Typesetting strings
******************************************************************************/

void
concater_rep::typeset_substring (string s, path ip, int pos) {
  box b= text_box (ip, pos, s, env->fn, env->col);
  a << line_item (STRING_ITEM, b, HYPH_INVALID, env->lan);
}

void
concater_rep::typeset_colored_substring
  (string s, path ip, int pos, string col)
{
  color c= (col == ""? env->col: named_color (col));
  box b= text_box (ip, pos, s, env->fn, c);
  a << line_item (STRING_ITEM, b, HYPH_INVALID, env->lan);
}

#define PRINT_SPACE(spc_type) \
  switch (spc_type) { \
  case SPC_NONE: \
    break; \
  case SPC_SPACE: \
    print (spc); \
    break; \
  case SPC_DSPACE: \
    print (space (spc->min << 1, spc->def << 1, spc->max << 1)); \
    break; \
  case SPC_PERIOD: \
    print (spc+ extra); \
    break; \
  case SPC_TINY: \
    print (space (spc->min>>2, spc->def>>2, spc->max>>2)); \
    break; \
  case SPC_OPERATOR: \
    print (space (spc->min>>1, spc->def>>1, spc->max)); \
    break; \
  case SPC_BIGOP: \
    print (spc); \
    break; \
  }

#define PRINT_CONDENSED_SPACE(spc_type) \
  switch (spc_type) { \
  case SPC_NONE: \
    break; \
  case SPC_SPACE: \
    print (spc); \
    break; \
  case SPC_PERIOD: \
    print (spc+ extra); \
    break; \
  case SPC_TINY: \
    print (space (spc->min>>4, spc->def>>4, spc->max>>4)); \
    break; \
  case SPC_OPERATOR: \
    print (space (spc->min>>3, spc->def>>3, spc->max>>2)); \
    break; \
  case SPC_BIGOP: \
    print (space (spc->min>>2, spc->def>>2, spc->max>>2)); \
    break; \
  }

void
concater_rep::typeset_text_string (tree t, path ip, int pos, int end) {
  string s= t->label;
  int    start;
  space  spc= env->fn->spc;
  space  extra= env->fn->extra;

  do {
    start= pos;
    text_property tp= env->lan->advance (t, pos);
    if (pos > end) pos= end;
    if ((pos>start) && (s[start]==' ')) { // spaces
      if (start==0) typeset_substring ("", ip, 0);
      penalty_min (tp->pen_after);
      PRINT_SPACE (tp->spc_before);
      PRINT_SPACE (tp->spc_after);
      if ((pos==end) || (s[pos]==' '))
	typeset_substring ("", ip, pos);
    }
    else { // strings
      penalty_max (tp->pen_before);
      PRINT_SPACE (tp->spc_before)
      typeset_substring (s (start, pos), ip, start);
      penalty_min (tp->pen_after);
      PRINT_SPACE (tp->spc_after)
    }
  } while (pos<end);
}

void
concater_rep::typeset_math_string (tree t, path ip, int pos, int end) {
  string s= t->label;
  int    start;
  space  spc= env->fn->spc;
  space  extra= env->fn->extra;
  bool   condensed= env->math_condensed;

  do {
    start= pos;
    text_property tp= env->lan->advance (t, pos);
    if (pos > end) pos= end;
    if ((pos>start) && (s[start]==' ')) { // spaces
      if (start==0) typeset_substring ("", ip, 0);
      penalty_min (tp->pen_after);
      PRINT_SPACE (tp->spc_before);
      PRINT_SPACE (tp->spc_after);
      if ((pos==end) || (s[pos]==' '))
	typeset_substring ("", ip, pos);
    }
    else { // strings
      penalty_max (tp->pen_before);
      if (condensed) PRINT_CONDENSED_SPACE (tp->spc_before)
      else PRINT_SPACE (tp->spc_before)
      typeset_substring (s (start, pos), ip, start);
      penalty_min (tp->pen_after);
      if (tp->limits != LIMITS_NONE) with_limits (tp->limits);
      if (condensed) PRINT_CONDENSED_SPACE (tp->spc_after)
      else PRINT_SPACE (tp->spc_after)
    }
  } while (pos<end);
}

void
concater_rep::typeset_prog_string (tree t, path ip, int pos, int end) {
  string s= t->label;
  int    start;
  space  spc= env->fn->spc;
  space  extra= env->fn->extra;

  do {
    start= pos;
    text_property tp= env->lan->advance (t, pos);
    if (pos > end) pos= end;
    if ((pos>start) && (s[start]==' ')) { // spaces
      if (start==0) typeset_substring ("", ip, 0);
      penalty_min (tp->pen_after);
      PRINT_SPACE (tp->spc_before);
      PRINT_SPACE (tp->spc_after);
      if ((pos==end) || (s[pos]==' '))
	typeset_substring ("", ip, pos);
    }
    else { // strings
      penalty_max (tp->pen_before);
      PRINT_SPACE (tp->spc_before)
      typeset_colored_substring (s (start, pos), ip, start,
				 env->lan->get_color (t, start, pos));
      penalty_min (tp->pen_after);
      PRINT_SPACE (tp->spc_after)
    }
  } while (pos<end);
}

/******************************************************************************
* Typesetting errors, documents and concatenations
******************************************************************************/

void
concater_rep::typeset_paragraph (tree t, path ip) {
  print (STD_ITEM, ::typeset_as_paragraph (env, t[0], descend (ip, 0)));
}

void
concater_rep::typeset_document (tree t, path ip) {
  print (STD_ITEM, ::typeset_as_stack (env, t, ip));
}

void
concater_rep::typeset_surround (tree t, path ip) {
  if (N(t) == 3) {
    marker (descend (ip, 0));
    typeset (t[0], descend (ip, 0));
    array<line_item> b= ::typeset_concat (env, t[1], descend (ip, 1));
    typeset (t[2], descend (ip, 2));
    a << b;
    marker (descend (ip, 1));
  }
  else typeset_error (t, ip);
}

void
concater_rep::typeset_concat (tree t, path ip) {
  int i, n= N(t);
  for (i=0; i<n; i++)
    typeset (t[i], descend (ip, i));
}

void
concater_rep::typeset_rigid (tree t, path ip) {
  if (N(t) == 1) {
    box b= typeset_as_concat (env, t[0], descend (ip, 0));
    print (STD_ITEM, move_box (ip, b, 0, 0, true));
  }
  else typeset_error (t, ip);
}

/******************************************************************************
* Typesetting space
******************************************************************************/

void
concater_rep::typeset_hspace (tree t, path ip) {
  if (N(t) == 1 || N(t) == 3) {
    if (N(a)==0) print (STD_ITEM, empty_box (ip, 0, 0, 0, env->fn->yx));
    if (N(t)==1) print (env->as_hspace (t[0]));
    else print (space (env->as_length (t[0]),
                       env->as_length (t[1]),
                       env->as_length (t[2])));
    control (t, ip);
  }
  else typeset_error (t, ip);  
}

void
concater_rep::typeset_space (tree t, path ip) {
  if (N(t) == 1 || N(t) == 3) {
    SI w = env->as_length (t[0]);
    SI y1= 0;
    SI y2= env->fn->yx;
    if (N(t)==3) {
      y1= env->as_length (t[1]);
      y2= env->as_length (t[2]);
    }
    print (STD_ITEM, empty_box (ip, 0, y1, w, y2));
  }
  else typeset_error (t, ip);  
}

/******************************************************************************
* Moving and resizing
******************************************************************************/

void
concater_rep::typeset_move (tree t, path ip) {
  if (N(t) == 3) {
    box  b  = typeset_as_concat (env, t[0], descend (ip, 0));
    tree old= env->local_begin_extents (b);
    SI   x  = (t[1] == ""? 0: env->as_length (env->exec (t[1]), "w"));
    SI   y  = (t[2] == ""? 0: env->as_length (env->exec (t[2]), "h"));
    env->local_end_extents (old);
    print (STD_ITEM, move_box (ip, b, x, y, true));
  }
  else typeset_error (t, ip);  
}

void
concater_rep::typeset_shift (tree t, path ip) {
  if (N(t) == 3) {
    box  b  = typeset_as_concat (env, t[0], descend (ip, 0));
    tree old= env->local_begin_extents (b);
    SI   x  = (t[1] == ""? 0: env->as_length (env->exec (t[1]), "w"));
    SI   y  = (t[2] == ""? 0: env->as_length (env->exec (t[2]), "h"));
    env->local_end_extents (old);
    print (STD_ITEM, shift_box (ip, b, x, y, true));
  }
  else typeset_error (t, ip);  
}

void
concater_rep::typeset_resize (tree t, path ip) {
  if (N(t) == 5) {
    box  b = typeset_as_concat (env, t[0], descend (ip, 0));
    tree old= env->local_begin_extents (b);
    SI   x1 = (t[1] == ""? b->x1: env->as_length (env->exec (t[1]), "w"));
    SI   y1 = (t[2] == ""? b->y1: env->as_length (env->exec (t[2]), "h"));
    SI   x2 = (t[3] == ""? b->x2: env->as_length (env->exec (t[3]), "w"));
    SI   y2 = (t[4] == ""? b->y2: env->as_length (env->exec (t[4]), "h"));
    env->local_end_extents (old);
    print (STD_ITEM, resize_box (ip, b, x1, y1, x2, y2, true, true));
  }
  else typeset_error (t, ip);  
}

void
concater_rep::typeset_clipped (tree t, path ip) {
  if (N(t) == 5) {
    box  b = typeset_as_concat (env, t[0], descend (ip, 0));
    tree old= env->local_begin_extents (b);
    SI   x1 = (t[1] == ""? b->x1: env->as_length (env->exec (t[1]), "w"));
    SI   y1 = (t[2] == ""? b->y1: env->as_length (env->exec (t[2]), "h"));
    SI   x2 = (t[3] == ""? b->x2: env->as_length (env->exec (t[3]), "w"));
    SI   y2 = (t[4] == ""? b->y2: env->as_length (env->exec (t[4]), "h"));
    env->local_end_extents (old);
    print (STD_ITEM, clip_box (ip, b, x1, y1, x2, y2));
  }
  else typeset_error (t, ip);  
}

/******************************************************************************
* Floating objects
******************************************************************************/

void
concater_rep::typeset_float (tree t, path ip) {
  if (N(t) == 3) {
    tree t1= env->exec (t[0]);
    tree t2= env->exec (t[1]);
    tree ch= tuple (t1, t2);
    lazy lz= make_lazy_vstream (env, t[2], descend (ip, 2), ch);
    marker (descend (ip, 0));
    if (is_accessible (ip) && !env->read_only)
      flag_ok (as_string (t1), decorate_middle (ip), brown);
    print (FLOAT_ITEM, control_box (decorate_middle (ip), lz, env->fn));
    marker (descend (ip, 1));
  }
  else typeset_error (t, ip);  
}

/******************************************************************************
* Repetition of boxes inside other boxes
******************************************************************************/

void
concater_rep::typeset_repeat (tree t, path ip) {
  if (N(t) == 2) {
    box b1  = typeset_as_concat (env, t[0], descend (ip, 0));
    box b2  = typeset_as_concat (env, t[1], descend (ip, 1));
    SI  xoff= env->get_length (XOFF_DECORATIONS);
    print (STD_ITEM, repeat_box (ip, b1, b2, xoff));
  }
  else typeset_error (t, ip);  
}

/******************************************************************************
* Formatting information and decorations
******************************************************************************/

void
concater_rep::typeset_formatting (tree t, path ip, string v) {
  int n= N(t);
  tree new_format= env->read (v) * t (0, n-1);
  tree old_format= env->local_begin (v, new_format);
  if (v != CELL_FORMAT) {
    marker (descend (ip, 0));
    control (t (0, N(t)-1), decorate (ip));
  }
  typeset (t[n-1], descend (ip, n-1));
  if (v != CELL_FORMAT) {
    control (tree (L(t)), decorate (ip));
    marker (descend (ip, 1));
  }
  env->local_end (v, old_format);
}

void
concater_rep::typeset_decorated_box (tree t, path ip) {
  (void) t; (void) ip;
  int n= N (env->decorated_boxes);
  if ((n > 0) && (!is_nil (env->decorated_boxes [n-1]))) {
    print (STD_ITEM, env->decorated_boxes [n-1]);
    env->decorated_boxes [n-1]= box ();
  }
}
