
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
#include "packrat.hpp"

/******************************************************************************
* Typesetting special mathematical symbols
******************************************************************************/

void
concater_rep::typeset_large (tree t, path ip, int tp, int otp, string prefix) {
  font old_fn= env->fn;
  if (starts (old_fn->res_name, "stix-"))
    //if (old_fn->type == FONT_TYPE_UNICODE)
    env->fn= rubber_font (old_fn);
  
  if (N(t) < 1 || !is_atomic (t[0]))
    typeset_error (t, ip);
  else {
    string br= t[0]->label;
    if (N(br) > 2 && br[0] == '<' && br[N(br)-1] == '>')
      br= br (1, N(br) - 1);
    if (N(t) == 1) {
      string s= prefix * br * ">";
      box b= text_box (ip, 0, s, env->fn, env->pen);
      print (tp, otp, b);
      // temporarary: use parameters from group-open class in std-math.syx
      // bug: allow hyphenation after ) and before *
    }
    else if (N(t) == 2 && is_int (t[1])) {
      int nr= max (as_int (t[1]->label), 0);
      string s= prefix * br * "-" * as_string (nr) * ">";
      box b= text_box (ip, 0, s, env->fn, env->pen);
      SI dy= env->fn->yfrac - ((b->y1 + b->y2) >> 1);
      box mvb= move_box (ip, b, 0, dy, false, true);
      print (STD_ITEM, otp, macro_box (ip, mvb, env->fn));
    }
    else {
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
      string s= prefix * br * ">";
      box b= delimiter_box (ip, s, env->fn, env->pen, y1, y2);
      print (STD_ITEM, otp, b);
    }
  }

  env->fn= old_fn;
}

inline array<space>
get_spacing (font fn, int id, bool condensed, bool display) {
  if (condensed) return fn->get_narrow_spacing (id);
  if (display) return fn->get_wide_spacing (id);
  return fn->get_normal_spacing (id);
}

void
concater_rep::typeset_wide_middle (tree t, path ip) {
  array<space> spc_tab=
    get_spacing (env->fn, env->spacing_policy, env->math_condensed,
                 env->display_style && env->nesting_level == 0);
  space spc= spc_tab[SPC_MIDDLE];
  if (spc->max > 0) print (spc);
  typeset_large (t, ip, MIDDLE_BRACKET_ITEM, OP_MIDDLE_BRACKET, "<mid-");
  if (spc->max > 0) print (spc);
}

static void
get_big_flags (string l, bool& int_flag, bool& it_flag, bool& lim_flag) {
  int n= N(l);
  if (n < 3) return;
  if (l[n-3] == 'l' && l[n-2] == 'i' && l[n-1] == 'm') {
    l= l (0, n-3);
    n -= 3;
    if (l[n-3] != 'i' || l[n-2] != 'n' || l[n-1] != 't') return;
    int_flag= true;
    it_flag = l[0] != 'u' || l[1] != 'p';
    lim_flag= true;
  }
  else {
    if (l[n-3] != 'i' || l[n-2] != 'n' || l[n-1] != 't') return;
    int_flag= true;
    it_flag = l[0] != 'u' || l[1] != 'p';
    lim_flag= false;
  }
}

void
concater_rep::typeset_bigop (tree t, path ip) {
  if ((N(t) == 1) && is_atomic (t[0])) {
    space spc= env->fn->spc;
    string l= t[0]->label;
    string s= "<big-" * l * ">";
    bool flag= (!env->math_condensed) && (l != ".");
    box b;
    if (env->fn->type == FONT_TYPE_UNICODE) {
      font mfn= rubber_font (env->fn);
      b= big_operator_box (ip, s, mfn, env->pen,
                           env->display_style? 2: 1);
    }
    else b= big_operator_box (ip, s, env->fn, env->pen,
                              env->display_style? 2: 1);
    print (STD_ITEM, OP_BIG, b);
    penalty_min (HYPH_PANIC);
    bool int_flag= false, it_flag= false, lim_flag= true;
    get_big_flags (l, int_flag, it_flag, lim_flag);
    if (lim_flag) with_limits (LIMITS_DISPLAY);
    if (flag) {
      if (int_flag) {
        if (env->fn->math_type == MATH_TYPE_STIX)
          print (env->display_style? (spc / 2): (spc / 4));
        else if (env->fn->math_type == MATH_TYPE_TEX_GYRE)
          print (env->display_style? (spc / 2): (spc / 4));
        else if (it_flag)
          print (env->display_style? 0: (spc / 4));
        else print (spc / 4);
      }
      else print (env->display_style? spc: (spc / 2));
    }
    // FIXME: we should use parameters from operator-big class in std-math.syx
    // FIXME: in concat_post, we add some more space behind big operators
    //        with scripts; this should be understood better and formalized
  }
  else typeset_error (t, ip);
}

string
replace_primes (string s) {
  string r;
  int i, n= N(s);
  for (i=0; i<n; i++)
    if (s[i] == '\'') r << "<prime>";
    else if (s[i] == '`') r << "<backprime>";
    else r << s[i];
  return r;
}

void
concater_rep::typeset_lprime (tree t, path ip) {
  if ((N(t) == 1) && is_atomic (t[0])) {
    string s= t[0]->label;
    bool flag= (env->fn->type == FONT_TYPE_UNICODE);
    if (flag)
      for (int i=0; i<N(s); i++)
        flag= flag && (s[i] == '\'' || s[i] == '`');
    if (env->fn->type == FONT_TYPE_TEX ||
        env->fn->math_type != MATH_TYPE_NORMAL)
      s= replace_primes (s);
    tree old_il;
    if (!flag) old_il= env->local_begin_script ();
    path sip= descend (ip, 0);
    box b1, b2;
    b2= typeset_as_concat (env, s /*t[0]*/, sip);
    b2= symbol_box (sip, b2, N(t[0]->label));
    if (flag || env->fn->math_type != MATH_TYPE_TEX_GYRE)
      b2= move_box (sip, b2,
                    flag? 0: env->as_length (string ("-0.05fn")),
                    flag? env->as_length ("-0.75ex"): 0,
                    false, true);
    if (!flag) env->local_end_script (old_il);
    print (LSUP_ITEM, OP_SKIP, script_box (ip, b1, b2, env->fn));
    penalty_max (HYPH_INVALID);
  }
  else typeset_error (t, ip);
}

void
concater_rep::typeset_rprime (tree t, path ip) {
  if ((N(t) == 1) && is_atomic (t[0])) {
    string s= t[0]->label;
    bool flag= (env->fn->type == FONT_TYPE_UNICODE);
    if (flag)
      for (int i=0; i<N(s); i++)
	flag= flag && (s[i] == '\'' || s[i] == '`');
    if (env->fn->type == FONT_TYPE_TEX ||
        env->fn->math_type != MATH_TYPE_NORMAL)
      s= replace_primes (s);
    tree old_il;
    if (!flag) old_il= env->local_begin_script ();
    path sip= descend (ip, 0);
    box b1, b2;
    b2= typeset_as_concat (env, s /*t[0]*/, sip);
    if (b2->right_slope () == 0.25001 && b2->y1 > 0)
      // NOTE: hack for detection of poor italic font
      b2= shift_box (sip, b2, (SI) (-1.0 * b2->y1 * b2->right_slope ()), 0);
    b2= symbol_box (sip, b2, N(t[0]->label));
    if (flag || env->fn->math_type != MATH_TYPE_TEX_GYRE)
      b2= move_box (sip, b2,
                    flag? 0: env->as_length (string ("0.05fn")),
                    flag? env->as_length ("-0.75ex"): 0,
                    false, true);
    if (!flag) env->local_end_script (old_il);
    penalty_max (HYPH_INVALID);
    if (N(a)>0) a[N(a)-1]->limits= false;
    print (RSUP_ITEM, OP_SKIP, script_box (ip, b1, b2, env->fn));
  }
  else typeset_error (t, ip);
}

/******************************************************************************
* Typesetting scripts
******************************************************************************/

void
concater_rep::typeset_long_arrow (tree t, path ip) {
  if (N(t) != 2 && N(t) != 3) { typeset_error (t, ip); return; }
  tree old_ds= env->local_begin (MATH_DISPLAY, "false");
  tree old_mc= env->local_begin (MATH_CONDENSED, "true");
  tree old_il= env->local_begin_script ();
  box sup_b, sub_b;
  if (N(t) >= 2) {
    tree old_vp= env->local_begin (MATH_VPOS, "-1");
    sup_b= typeset_as_concat (env, t[1], descend (ip, 1));
    env->local_end (MATH_VPOS, old_vp);
  }
  if (N(t) >= 3) {
    tree old_vp= env->local_begin (MATH_VPOS, "1");
    sub_b= typeset_as_concat (env, t[2], descend (ip, 2));
    env->local_end (MATH_VPOS, old_vp);
  }
  env->local_end_script (old_il);
  env->local_end (MATH_CONDENSED, old_mc);
  env->local_end (MATH_DISPLAY, old_ds);

  string s= env->exec_string (t[0]);
  SI w= sup_b->w();
  if (N(t) == 3) w= max (w, sub_b->w());
  w += env->fn->wquad;
  box arrow= wide_box (decorate (descend (ip, 0)), s, env->fn, env->pen, w);

  space spc= env->fn->spc;
  if (env->math_condensed) spc= space (spc->min>>3, spc->def>>3, spc->max>>2);
  else spc= space (spc->min>>1, spc->def>>1, spc->max);
  print (spc);
  print (limit_box (ip, arrow, sub_b, sup_b, env->fn, false));
  print (spc);
}

void
concater_rep::typeset_below (tree t, path ip) {
  if (N(t) != 2) { typeset_error (t, ip); return; }
  box b1= typeset_as_concat (env, t[0], descend (ip, 0));
  tree old_ds= env->local_begin (MATH_DISPLAY, "false");
  tree old_mc= env->local_begin (MATH_CONDENSED, "true");
  tree old_il= env->local_begin_script ();
  box b2= typeset_as_concat (env, t[1], descend (ip, 1));
  env->local_end_script (old_il);
  env->local_end (MATH_CONDENSED, old_mc);
  env->local_end (MATH_DISPLAY, old_ds);
  print (limit_box (ip, b1, b2, box (), env->fn, false));
}

void
concater_rep::typeset_above (tree t, path ip) {
  if (N(t) != 2) { typeset_error (t, ip); return; }
  box b1= typeset_as_concat (env, t[0], descend (ip, 0));
  tree old_ds= env->local_begin (MATH_DISPLAY, "false");
  tree old_mc= env->local_begin (MATH_CONDENSED, "true");
  tree old_il= env->local_begin_script ();
  box b2= typeset_as_concat (env, t[1], descend (ip, 1));
  env->local_end_script (old_il);
  env->local_end (MATH_CONDENSED, old_mc);
  env->local_end (MATH_DISPLAY, old_ds);
  // NOTE: start dirty hack to get scripts above ... right
  if ((t[0] == "<ldots>" && env->read ("low-dots") != UNINIT) ||
      (t[0] == "<cdots>" && env->read ("center-dots") != UNINIT)) {
    string s= (t[0] == "<ldots>"? ",": "<cdot>");
    box tb= typeset_as_concat (env, s, decorate_middle (descend (ip, 0)));
    b1= resize_box (descend (ip, 0), b1, b1->x1, b1->y1, b1->x2, tb->y2);
  }
  // NOTE: end dirty hack to get scripts above ... right
  print (limit_box (ip, b1, box (), b2, env->fn, false));
}

void
concater_rep::typeset_script (tree t, path ip, bool right) {
  if (N(t) != 1) { typeset_error (t, ip); return; }
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
  a << line_item (type, OP_SKIP,
                  script_box (ip, b1, b2, env->fn), HYPH_INVALID);
  // do not use print, because of italic space
  if (!right) penalty_max (HYPH_INVALID);
}

/******************************************************************************
* Standard mathematical operations
******************************************************************************/

static bool
needs_brackets (tree t, string kind) {
  if (is_func (t, AROUND)) return false;
  return !packrat_correct ("std-math", kind, t);
}

void
concater_rep::typeset_wide_frac (tree t, path ip) {
  bool numb= needs_brackets (t[0], "Product");
  bool denb= needs_brackets (t[1], "Power");
  pencil old_pen= env->pen;
  marker (descend (ip, 0));
  typeset_large (tree (LEFT, "."), decorate_left (descend (ip, 0)),
                 LEFT_BRACKET_ITEM, OP_OPENING_BRACKET, "<left-");
  env->pen= env->flatten_pen;
  if (numb)
    typeset_large (tree (LEFT, "("), decorate_left (descend (ip, 0)),
                   LEFT_BRACKET_ITEM, OP_OPENING_BRACKET, "<left-");
  env->pen= old_pen;
  typeset (t[0], descend (ip, 0));
  env->pen= env->flatten_pen;
  if (numb)
    typeset_large (tree (RIGHT, ")"), decorate_right (descend (ip, 0)),
                   RIGHT_BRACKET_ITEM, OP_CLOSING_BRACKET, "<right-");
  typeset_large (tree (MID, "/"), decorate_middle (ip),
                 MIDDLE_BRACKET_ITEM, OP_MIDDLE_BRACKET, "<mid-");
  if (denb)
    typeset_large (tree (LEFT, "("), decorate_left (descend (ip, 1)),
                   LEFT_BRACKET_ITEM, OP_OPENING_BRACKET, "<left-");
  env->pen= old_pen;
  typeset (t[1], descend (ip, 1));
  env->pen= env->flatten_pen;
  if (denb)
    typeset_large (tree (RIGHT, ")"), decorate_right (descend (ip, 1)),
                   RIGHT_BRACKET_ITEM, OP_CLOSING_BRACKET, "<right-");
  env->pen= old_pen;
  typeset_large (tree (RIGHT, "."), decorate_right (descend (ip, 1)),
                 RIGHT_BRACKET_ITEM, OP_CLOSING_BRACKET, "<right-");
  marker (descend (ip, 1));
}

void
concater_rep::typeset_frac (tree t, path ip) {
  if (N(t) != 2) { typeset_error (t, ip); return; }
  bool disp= env->display_style;
  tree old;
  if (disp) old= env->local_begin (MATH_DISPLAY, "false");
  else old= env->local_begin_script ();
  tree old_vp= env->local_begin (MATH_VPOS, "1");
  box num= typeset_as_concat (env, t[0], descend (ip, 0));
  env->local_end (MATH_VPOS, "-1");
  box den= typeset_as_concat (env, t[1], descend (ip, 1));
  env->local_end (MATH_VPOS, old_vp);
  font sfn= env->fn;
  if (disp) env->local_end (MATH_DISPLAY, old);
  else env->local_end_script (old);
  if (num->w() <= env->frac_max && den->w () <= env->frac_max)
    print (frac_box (ip, num, den, env->fn, sfn, env->pen));
  else typeset_wide_frac (t, ip);
}

void
concater_rep::typeset_wide_sqrt (tree t, path ip) {
  bool br= needs_brackets (t[0], "Postfixed");
  pencil old_pen= env->pen;
  marker (descend (ip, 0));
  typeset_large (tree (LEFT, "."), decorate_left (descend (ip, 0)),
                 LEFT_BRACKET_ITEM, OP_OPENING_BRACKET, "<left-");
  env->pen= env->flatten_pen;
  if (br)
    typeset_large (tree (LEFT, "("), decorate_left (descend (ip, 0)),
                   LEFT_BRACKET_ITEM, OP_OPENING_BRACKET, "<left-");
  env->pen= old_pen;
  typeset (t[0], descend (ip, 0));
  env->pen= env->flatten_pen;
  if (br)
    typeset_large (tree (RIGHT, ")"), decorate_right (descend (ip, 0)),
                   RIGHT_BRACKET_ITEM, OP_CLOSING_BRACKET, "<right-");
  env->pen= old_pen;

  bool disp= env->display_style;
  tree old;
  if (disp) old= env->local_begin (MATH_DISPLAY, "false");
  tree old_il= env->local_begin_script ();
  env->pen= env->flatten_pen;
  box num= typeset_as_concat (env, "1", decorate_middle (ip));
  box den;
  if (N(t) >= 2) {
    env->pen= old_pen;
    den= typeset_as_concat (env, t[1], descend (ip, 1));
    env->pen= env->flatten_pen;
  }
  else den= typeset_as_concat (env, "2", decorate_middle (ip));
  box fr= frac_box (decorate_middle (ip), num, den, env->fn, env->fn, env->pen);
  env->pen= old_pen;
  env->local_end_script (old_il);
  if (disp) env->local_end (MATH_DISPLAY, old);
  penalty_max (HYPH_INVALID);
  a << line_item (RSUP_ITEM, OP_SKIP,
                  script_box (ip, box (), fr, env->fn), HYPH_INVALID);

  typeset_large (tree (RIGHT, "."), decorate_right (descend (ip, 1)),
                 RIGHT_BRACKET_ITEM, OP_CLOSING_BRACKET, "<right-");
  marker (descend (ip, 1));
}

void
concater_rep::typeset_sqrt (tree t, path ip) {
  if (N(t) != 1 && N(t) != 2) { typeset_error (t, ip); return; }
  box b= typeset_as_concat (env, t[0], descend (ip, 0));
  if (b->w () > env->frac_max) { typeset_wide_sqrt (t, ip); return; }
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
  font lfn= env->fn;
  bool stix= starts (lfn->res_name, "stix-");
  if (stix) lfn= rubber_font (lfn);
  box sqrtb= delimiter_box (decorate_left (ip), "<large-sqrt>",
                            lfn, env->pen, b->y1, b->y2 + (3*sep >> 1));
  if (stix) sqrtb= shift_box (decorate_left (ip), sqrtb,
                              -env->fn->wline/2, -env->fn->wline/3,
                              false, true);
  print (sqrt_box (ip, b, ind, sqrtb, env->fn, env->pen));
}

void
concater_rep::typeset_wide (tree t, path ip, bool above) {
  if (N(t) != 2) { typeset_error (t, ip); return; }
  box b= typeset_as_concat (env, t[0], descend (ip, 0));
  string s= env->exec_string (t[1]);
  if (s == "^") s= "<hat>";
  if (s == "~") s= "<tilde>";
  bool request_wide= false;
  if (starts (s, "<wide-")) {
    s= "<" * s (6, N(s));
    request_wide= true;
  }
  if (ends (s, "brace>") || ends (s, "brace*>"))
    b= move_box (decorate_middle (descend (ip, 0)), b, 0, 0, true);
  box wb= wide_box (ip, b, s, env->fn, env->pen, request_wide, above);
  print_semantic (wb, t[0]);
  if (ends (s, "brace>")) with_limits (LIMITS_ALWAYS);
}

void
concater_rep::typeset_neg (tree t, path ip) {
  if (N(t) != 1) { typeset_error (t, ip); return; }
  box b= typeset_as_concat (env, t[0], descend (ip, 0));
  print_semantic (neg_box (ip, b, env->fn, env->pen), t[0]);
}

/******************************************************************************
* Other markup
******************************************************************************/

static string
bracket_color (int nl) {
  switch (nl % 3) {
  case 0 : return "#662266";
  case 1 : return "#226666";
  default: return "#663322";
  }
}

static tree
make_large (tree_label l, tree t) {
  if (!is_atomic (t)) {
    if (is_func (t, l)) {
      if (N(t) == 2 && is_atomic (t[0]) && is_int (t[1])) {
        string s= t[0]->label;
        if (N(s) >= 3 && s[0] == '<' && s[N(s)-1] == '>') s= s (1, N(s)-1);
        return tree (l, s * "-" * t[1]->label);
      }
      else return t;
    }
    else return tree (l, ".");
  }
  string s= t->label;
  if (N(s) <= 1) return tree (l, s);
  if (s[0] != '<' || s[N(s)-1] != '>' || s == "<nobracket>")
    return tree (l, ".");
  return tree (l, s (1, N(s)-1));
}

void
concater_rep::typeset_around (tree t, path ip, bool colored) {
  tree old_nl=
    env->local_begin (MATH_NESTING_LEVEL, as_string (env->nesting_level + 1));
  if (colored) {
    tree old_col= env->local_begin (COLOR, bracket_color (env->nesting_level));
    typeset_around (t, ip, false);
    env->local_end (COLOR, old_col);
  }
  else {
    marker (descend (ip, 0));
    switch (L(t)) {
    case AROUND:
      if (N(t) == 3) {
        box br1= typeset_as_concat (env, t[0], descend (ip, 0));
        print (STD_ITEM, OP_OPENING_BRACKET, br1);
        typeset (t[1], descend (ip, 1));
        box br2= typeset_as_concat (env, t[2], descend (ip, 2));
        print (STD_ITEM, OP_CLOSING_BRACKET, br2);
      }
      else typeset_error (t, ip);
      break;
    case VAR_AROUND:
      if (N(t) == 3) {
        font old_fn= env->fn;
        font new_fn= env->fn;
        if (starts (new_fn->res_name, "stix-"))
          //if (new_fn->type == FONT_TYPE_UNICODE)
          new_fn= rubber_font (new_fn);
        env->fn= new_fn;
        typeset (make_large (LEFT, t[0]),
                 decorate_middle (descend (ip, 0)));
        env->fn= old_fn;
        typeset (t[1], descend (ip, 1));
        env->fn= new_fn;
        typeset (make_large (RIGHT, t[2]),
                 decorate_middle (descend (ip, 2)));
        env->fn= old_fn;
      }
      else typeset_error (t, ip);
      break;
    case BIG_AROUND:
      if (N(t) == 2) {
        typeset (make_large (BIG, t[0]),
                 decorate_middle (descend (ip, 0)));
        typeset (t[1], descend (ip, 1));
      }
      else typeset_error (t, ip);
      break;
    default:
      break;
    }
    marker (descend (ip, 1));
  }
  env->local_end (MATH_NESTING_LEVEL, old_nl);
}

void
concater_rep::typeset_tree (tree t, path ip) {
  if (N(t) == 0) { typeset_error (t, ip); return; }
  int i, n= N(t);
  array<box> bs(n);
  for (i=0; i<n; i++) bs[i]= typeset_as_concat (env, t[i], descend (ip, i));
  print (tree_box (ip, bs, env->fn, env->pen));
}

void
concater_rep::typeset_table (tree t, path ip) {
  box b= typeset_as_table (env, t, ip);
  if (b->w () <= env->table_max) { print (b); return; }
  if (env->read (TABLE_WIDTH) != "") { print (b); return; }
  path ip1= ip;
  tree t1 = t;
  while (is_func (t1, TFORMAT)) {
    ip1= descend (ip1, N(t1)-1);
    t1 = t1[N(t1)-1];
  }
  for (int i=0; i<N(t1); i++) {
    tree t2= t1[i];
    if (!is_func (t2, ROW)) { print (b); return; }
    for (int j=0; j<N(t2); j++) {
      tree t3= t2[j];
      if (!is_func (t3, CELL, 1)) { print (b); return; }
      if (is_func (t3[0], DOCUMENT)) { print (b); return; }
    }
  }

  pencil old_pen= env->pen;
  for (int i=0; i<N(t1); i++) {
    path ip2= descend (ip1, i);
    tree t2 = t1[i];
    for (int j=0; j<N(t2); j++) {
      path ip3= descend (descend (ip2, j), 0);
      tree t3 = t2[j][0];
      typeset (t3, ip3);
      if (i < N(t1)-1 || j < N(t2)-1) {
        string delim= ",";
        if (j == N(t2)-1) delim= ";";
        env->pen= env->flatten_pen;
        typeset (delim, decorate_right (ip3));
        env->pen= old_pen;
      }
    }
  }
}
