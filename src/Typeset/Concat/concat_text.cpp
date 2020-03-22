
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
#include "boot.hpp"

lazy make_lazy_vstream (edit_env env, tree t, path ip, tree channel);

/******************************************************************************
* Typesetting strings
******************************************************************************/

void
concater_rep::typeset_substring (string s, path ip, int pos) {
  box b= text_box (ip, pos, s, env->fn, env->pen);
  a << line_item (STRING_ITEM, OP_TEXT, b, HYPH_INVALID, env->lan);
}

void
concater_rep::typeset_math_substring (string s, path ip, int pos, int otype) {
  box b= text_box (ip, pos, s, env->fn, env->pen);
  a << line_item (STRING_ITEM, otype, b, HYPH_INVALID, env->lan);
}

void
concater_rep::typeset_math_macro
  (string s, tree_label m, path ip, int p1, int p2, int otype)
{
  // NOTE: start dirty hack to get spacing of ,..., right
  static tree_label LOW_DOTS= make_tree_label ("low-dots");
  static tree_label VAR_LOW_DOTS= make_tree_label ("low-dots*");
  if (m == LOW_DOTS) {
    char c1= (p1>0? s[p1-1]: ' ');
    char c2= (p2<N(s)? s[p2]: ' ');
    if (c1 == ',' || c1 == ':' || c1 == ';' ||
        c2 == ',' || c2 == ':' || c2 == ';') {
      m= VAR_LOW_DOTS;
      a[N(a)-1]->spc= 0;
    }
  }
  // NOTE: end dirty hack to get spacing of ,..., right

  marker (descend (ip, p1));
  typeset_compound (tree (m), decorate_middle (ip));
  marker (descend (ip, p2));
}

void
concater_rep::typeset_colored_substring
  (string s, path ip, int pos, string col)
{
  color c= (col == ""? env->pen->get_color (): named_color (col));
  if (env->alpha != 255) {
    int r, g, b, a;
    get_rgb_color (c, r, g, b, a);
    c= rgb_color (r, g, b, env->alpha);
  }
  box b= text_box (ip, pos, s, env->fn, c);
  a << line_item (STRING_ITEM, OP_TEXT, b, HYPH_INVALID, env->lan);
}

#define PRINT_SPACE(spc_type) \
  if (spc_type != SPC_NONE) print (spc_tab[spc_type]);

void
concater_rep::typeset_text_string (tree t, path ip, int pos, int end) {
  array<space> spc_tab= env->fn->get_normal_spacing (env->spacing_policy);
  string s= t->label;
  int    start;

  do {
    start= pos;
    text_property tp= env->lan->advance (t, pos);
    if (pos > end) pos= end;
    if ((pos > start) && (s[start] == ' ')) { // spaces
      if (start == 0) typeset_substring ("", ip, 0);
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

inline array<space>
get_spacing (font fn, int id, bool condensed, bool display) {
  if (condensed) return fn->get_narrow_spacing (id);
  if (display) return fn->get_wide_spacing (id);
  return fn->get_normal_spacing (id);
}

void
concater_rep::typeset_math_string (tree t, path ip, int pos, int end) {
  array<space> spc_tab=
    get_spacing (env->fn, env->spacing_policy, env->math_condensed,
                 env->display_style && env->nesting_level == 0);
  string s= t->label;
  int    start;

  do {
    start= pos;
    text_property tp= env->lan->advance (t, pos);
    int k= N(a);
    while (k > 0 && a[k-1]->op_type == OP_SKIP) k--;
    int prev_op_type= (k == 0? OP_TEXT: a[k-1]->op_type);
    int succ_status= succession_status (prev_op_type, tp->op_type);
    //cout << "Succession [" << s (start, pos) << "] "
    //     << "(" << prev_op_type << ", " << tp->op_type << ")"
    //     << " -> " << succ_status << "\n";
    if ((succ_status & 1) != 0 && k > 0) a[k-1]->spc= space (0);
    bool spc_ok= (succ_status <= 1);
    if (pos > end) pos= end;
    if ((pos > start) && (s[start]==' ')) { // spaces
      if (start == 0) typeset_substring ("", ip, 0);
      penalty_max (HYPH_INVALID);
      //penalty_min (tp->pen_after);
      if (spc_ok) {
        PRINT_SPACE (tp->spc_before);
        PRINT_SPACE (tp->spc_after);
      }
      //if ((pos==end) || (s[pos]==' '))
      typeset_math_substring ("", ip, pos, OP_APPLY);
    }
    else { // strings
      penalty_max (tp->pen_before);
      if (spc_ok) { PRINT_SPACE (tp->spc_before); }
      if (pos > start && s[start] == '*' && env->info_level >= INFO_SHORT) {
        color c = rgb_color (160, 160, 255);
        box   tb= text_box (decorate (ip), 0, "<cdot>", env->fn, c);
        box   sb= specific_box (decorate (ip), tb, "screen", env->fn);
        box   mb= move_box (decorate (ip), sb, -tb->w()>>1, 0);
        box   rb= resize_box (decorate (ip), mb, 0, tb->y1, 0, tb->y2);
        a << line_item (STD_ITEM, OP_SKIP, rb, HYPH_INVALID);
      }
      if (tp->macro == 0 || env->read (as_string (tp->macro)) == UNINIT)
        typeset_math_substring (s (start, pos), ip, start, tp->op_type);
      else
        typeset_math_macro (s, tp->macro, ip, start, pos, tp->op_type);
      penalty_min (tp->pen_after);
      if (tp->limits != LIMITS_NONE) with_limits (tp->limits);
      if (spc_ok) { PRINT_SPACE (tp->spc_after); }
      else if (tp->op_type == OP_INFIX || tp->op_type == OP_PREFIX_INFIX)
        penalty_max (HYPH_INVALID);
    }
  } while (pos<end);
}

void
concater_rep::typeset_prog_string (tree t, path ip, int pos, int end) {
  array<space> spc_tab= env->fn->get_normal_spacing (env->spacing_policy);
  string s= t->label;
  int    start;

  do {
    start= pos;
    text_property tp= env->lan->advance (t, pos);
    if (pos > end) pos= end;
    if ((pos-start == 1) && (s[start]==' ')) { // spaces
      if (start == 0) typeset_substring ("", ip, 0);
      penalty_min (tp->pen_after);
      PRINT_SPACE (tp->spc_before);
      PRINT_SPACE (tp->spc_after);
      if ((pos==end) || (s[pos]==' '))
        typeset_substring ("", ip, pos);
    }
    else { // strings
      penalty_max (tp->pen_before);
      PRINT_SPACE (tp->spc_before)
      string color= env->lan->get_color (t, start, pos);
      string content= s (start, pos);
      // cout << "[" << start << "," << pos << ") "
      //      << content << " (" << color << ")" << LF;
      typeset_colored_substring (content, ip, start, color);
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
  print (::typeset_as_paragraph (env, t[0], descend (ip, 0)));
}

void
concater_rep::typeset_document (tree t, path ip) {
  print (::typeset_as_stack (env, t, ip));
}

void
concater_rep::typeset_surround (tree t, path ip) {
  if (N(t) != 3) { typeset_error (t, ip); return; }
  marker (descend (ip, 0));
  typeset (t[0], descend (ip, 0));
  array<line_item> b= ::typeset_concat (env, t[1], descend (ip, 1));
  typeset (t[2], descend (ip, 2));
  a << b;
  marker (descend (ip, 1));
}

void
concater_rep::typeset_concat (tree t, path ip) {
  int i, n= N(t);
  for (i=0; i<n; i++)
    typeset (t[i], descend (ip, i));
}

void
concater_rep::typeset_rigid (tree t, path ip) {
  if (N(t) != 1) { typeset_error (t, ip); return; }
  box b= typeset_as_concat (env, t[0], descend (ip, 0));
  print (move_box (ip, b, 0, 0, true));
}

void
concater_rep::typeset_hgroup (tree t, path ip) {
  if (N(t) != 1 && N(t) != 2) { typeset_error (t, ip); return; }
  marker (descend (ip, 0));
  int start= N(a);
  language old_lan= env->lan;
  env->lan= hyphenless_language (env->lan);
  typeset (t[0], descend (ip, 0));
  env->lan= old_lan;
  int end= N(a);
  marker (descend (ip, 1));
  for (int i=start; i+1<end; i++)
    a[i]->penalty= HYPH_INVALID;
}

void
concater_rep::print_semantic (box b, tree sem) {
  if (is_atomic (sem) && tm_string_length (sem->label) == 1) {
    array<space> spc_tab=
      get_spacing (env->fn, env->spacing_policy, env->math_condensed,
                   env->display_style && env->nesting_level == 0);
    int    pos= 0;
    string s= sem->label;
    text_property tp= env->lan->advance (s, pos);
    int k= N(a);
    while (k > 0 && a[k-1]->op_type == OP_SKIP) k--;
    int prev_op_type= (k == 0? OP_TEXT: a[k-1]->op_type);
    int succ_status= succession_status (prev_op_type, tp->op_type);
    if ((succ_status & 1) != 0 && k > 0) a[k-1]->spc= space (0);
    bool spc_ok= (succ_status <= 1);
    penalty_max (tp->pen_before);
    if (spc_ok) { PRINT_SPACE (tp->spc_before); }
    print (STD_ITEM, tp->op_type, b);
    penalty_min (tp->pen_after);
    if (tp->limits != LIMITS_NONE) with_limits (tp->limits);
    if (spc_ok) { PRINT_SPACE (tp->spc_after); }
  }
  else print (b);  
}

void
concater_rep::typeset_syntax (tree t, path ip) {
  if (N(t) != 2) { typeset_error (t, ip); return; }
  box b= typeset_as_concat (env, t[0], descend (ip, 0));
  b= move_box (ip, b, 0, 0, true);
  tree sem= env->exec (t[1]);
  print_semantic (b, sem);
}

/******************************************************************************
* Typesetting space
******************************************************************************/

void
concater_rep::typeset_hspace (tree t, path ip) {
  if (N(t) != 1 && N(t) != 3) { typeset_error (t, ip); return; }
  if (N(a)==0) print (empty_box (ip, 0, 0, 0, env->fn->yx));
  if (N(t)==1) print (env->as_hspace (t[0]));
  else print (space (env->as_length (t[0]),
                     env->as_length (t[1]),
                     env->as_length (t[2])));
  control (t, ip);
}

void
concater_rep::typeset_space (tree t, path ip) {
  if (N(t) != 1 && N(t) != 3) { typeset_error (t, ip); return; }
  SI w = env->as_length (t[0]);
  SI y1= 0;
  SI y2= env->fn->yx;
  if (N(t)==3) {
    y1= env->as_length (t[1]);
    y2= env->as_length (t[2]);
  }
  print (empty_box (ip, 0, y1, w, y2));
}

/******************************************************************************
* Moving and resizing
******************************************************************************/

void
concater_rep::typeset_move (tree t, path ip) {
  if (N(t) != 3) { typeset_error (t, ip); return; }
  box  b  = typeset_as_concat (env, t[0], descend (ip, 0));
  tree old= env->local_begin_extents (b);
  SI   x  = (t[1] == ""? 0: env->as_length (env->exec (t[1]), "w"));
  SI   y  = (t[2] == ""? 0: env->as_length (env->exec (t[2]), "h"));
  env->local_end_extents (old);
  print (move_box (ip, b, x, y, true));
}

void
concater_rep::typeset_shift (tree t, path ip) {
  if (N(t) != 3) { typeset_error (t, ip); return; }
  box  b  = typeset_as_concat (env, t[0], descend (ip, 0));
  tree old= env->local_begin_extents (b);
  SI   x  = (t[1] == ""? 0: env->as_length (env->exec (t[1]), "w"));
  SI   y  = (t[2] == ""? 0: env->as_length (env->exec (t[2]), "h"));
  env->local_end_extents (old);
  print (shift_box (ip, b, x, y, true));
}

void
concater_rep::typeset_resize (tree t, path ip) {
  if (N(t) != 5) { typeset_error (t, ip); return; }
  box  b = typeset_as_concat (env, t[0], descend (ip, 0));
  tree old= env->local_begin_extents (b);
  SI   x1 = (t[1] == ""? b->x1: env->as_length (env->exec (t[1]), "w"));
  SI   y1 = (t[2] == ""? b->y1: env->as_length (env->exec (t[2]), "h"));
  SI   x2 = (t[3] == ""? b->x2: env->as_length (env->exec (t[3]), "w"));
  SI   y2 = (t[4] == ""? b->y2: env->as_length (env->exec (t[4]), "h"));
  env->local_end_extents (old);
  print (resize_box (ip, b, x1, y1, x2, y2, true, true));
}

void
concater_rep::typeset_clipped (tree t, path ip) {
  if (N(t) != 5) { typeset_error (t, ip); return; }
  box  b = typeset_as_concat (env, t[0], descend (ip, 0));
  tree old= env->local_begin_extents (b);
  SI   x1 = (t[1] == ""? b->x1: env->as_length (env->exec (t[1]), "w"));
  SI   y1 = (t[2] == ""? b->y1: env->as_length (env->exec (t[2]), "h"));
  SI   x2 = (t[3] == ""? b->x2: env->as_length (env->exec (t[3]), "w"));
  SI   y2 = (t[4] == ""? b->y2: env->as_length (env->exec (t[4]), "h"));
  env->local_end_extents (old);
  print (clip_box (ip, b, x1, y1, x2, y2));
}

/******************************************************************************
* Floating objects
******************************************************************************/

void
concater_rep::typeset_float (tree t, path ip) {
  if (N(t) != 3) { typeset_error (t, ip); return; }
  tree t1= env->exec (t[0]);
  tree t2= env->exec (t[1]);
  if (!env->page_floats) t2= "h";
  tree ch= tuple (t1, t2);
  lazy lz= make_lazy_vstream (env, t[2], descend (ip, 2), ch);
  marker (descend (ip, 0));
  if (is_accessible (ip) && !env->read_only)
    flag_ok (as_string (t1), decorate_middle (ip), brown);
  print (FLOAT_ITEM, OP_SKIP, control_box (decorate_middle (ip), lz, env->fn));
  marker (descend (ip, 1));
}

/******************************************************************************
* Repetition of boxes inside other boxes
******************************************************************************/

void
concater_rep::typeset_repeat (tree t, path ip, bool under) {
  if (N(t) != 2) { typeset_error (t, ip); return; }
  box b1  = typeset_as_concat (env, t[0], descend (ip, 0));
  box b2  = typeset_as_concat (env, t[1], descend (ip, 1));
  SI  xoff= env->get_length (XOFF_DECORATIONS);
  print (repeat_box (ip, b1, b2, xoff, under));
}

/******************************************************************************
* Formatting information and decorations
******************************************************************************/

void
concater_rep::typeset_formatting (tree t, path ip, string v) {
  if (N(t) == 0) { typeset_error (t, ip); return; }
  if (rigid && v == ATOM_DECORATIONS) {
    int k, n=N(t);
    box b= typeset_as_concat (env, t[n-1], descend (ip, n-1));
    tree e (DBOX);
    for (k=n-2; k>=0; k--)
      if (is_func (t[k], MACRO, 2))
        e= tree (COMPOUND, t[k], e);
    if (e != tree (DBOX)) {
      env->decorated_boxes << b;
      box bb= typeset_as_concat (env, attach_middle (e, ip));
      env->decorated_boxes->resize (N (env->decorated_boxes) - 1);
      b= bb;
    }
    marker (descend (ip, 0));
    print (b);
    marker (descend (ip, 1));
  }
  else {
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
}

void
concater_rep::typeset_decorated_box (tree t, path ip) {
  (void) t; (void) ip;
  int n= N (env->decorated_boxes);
  if ((n > 0) && (!is_nil (env->decorated_boxes [n-1]))) {
    print (env->decorated_boxes [n-1]);
    env->decorated_boxes [n-1]= box ();
  }
}

/******************************************************************************
* Marginal notes and page notes
******************************************************************************/

void
concater_rep::typeset_line_note (tree t, path ip) {
  box  b= typeset_as_concat (env, t[0], decorate (ip));
  box  c= control_box (decorate_middle (ip), b, env->fn);
  SI   x= env->as_length (env->exec (t[1]));
  SI   y= env->as_length (env->exec (t[2]));
  tree p= tuple (as_string (x), as_string (y));
  marker (descend (ip, 0));
  a << line_item (NOTE_LINE_ITEM, OP_SKIP, c, HYPH_INVALID, p);
  flag ("line note", ip, brown);
  marker (descend (ip, 1));
}

void
concater_rep::typeset_page_note (tree t, path ip) {
  box  b= typeset_as_concat (env, t[0], decorate (ip));
  box  c= control_box (decorate_middle (ip), b, env->fn);
  SI   x= env->as_length (env->exec (t[1]));
  SI   y= env->as_length (env->exec (t[2]));
  tree p= tuple (as_string (x), as_string (y));
  marker (descend (ip, 0));
  a << line_item (NOTE_PAGE_ITEM, OP_SKIP, c, HYPH_INVALID, p);
  flag ("page note", ip, brown);
  marker (descend (ip, 1));
}

/******************************************************************************
* Special additional content in case of page breaks
******************************************************************************/

void
concater_rep::typeset_if_page_break (tree t, path ip) {
  if (N(t) != 2) { typeset_error (t, ip); return; }
  tree pos= env->exec (t[0]);
  space spc= env->get_vspace (PAR_PAR_SEP);
  tree sep= tree (TMLEN, as_string (spc->min),
                         as_string (spc->def), as_string (spc->max));
  tree ch= tuple ("if-page-break", pos, sep);
  lazy lz= make_lazy_vstream (env, t[1], descend (ip, 1), ch);
  marker (descend (ip, 0));
  flag ("if-page-break", ip, brown);
  if (get_user_preference ("new style page breaking") != "off")
    print (FLOAT_ITEM, OP_SKIP,
           control_box (decorate_middle (ip), lz, env->fn));
  marker (descend (ip, 1));
}
