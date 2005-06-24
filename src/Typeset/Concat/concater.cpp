
/******************************************************************************
* MODULE     : concater.cpp
* DESCRIPTION: First pass for typesetting paragraphs;
*              an array of line_items is created of the right types.
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "concater.hpp"
#include "analyze.hpp"
#include "file.hpp"

/******************************************************************************
* Printing items
******************************************************************************/

SI italic_correction (box left, box right);

void
concater_rep::print (int type, box b) {
  a << line_item (type, b, HYPH_INVALID);
}

void
concater_rep::control (tree t, path ip) {
  box b= empty_box (ip, 0, 0, 0, env->fn->yx);
  a << line_item (CONTROL_ITEM, b, HYPH_INVALID, t);
}

void
concater_rep::marker (path ip) {
  if (is_decoration (ip)) ip= path (0, ip);
  string fn_name= "cmr";
  int sz= script (env->fn_size, env->index_level);
  font gfn (tex_font (env->dis, fn_name, sz, (int) (env->magn*env->dpi)));
  box b= text_box (ip->next, ip->item, "", gfn, env->dis->blue);
  a << line_item (STD_ITEM, b, HYPH_INVALID);
}

void
concater_rep::ghost (string s, path ip) {
  ghost (s, ip, env->dis->blue);
}

void
concater_rep::ghost (string s, path ip, color col) {
  if ((N(s)>2) && (s[0]=='<') && (s[N(s)-1]=='>')) {
    ghost ("<", ip, col);
    ghost (s (1,N(s)-1), ip, col);
    ghost (">", ip, col);
    return;
  }
  
  string fn_name= "cmr";
  if (N(s)==1) {
    if (s[0] == '<') { fn_name= "cmsy"; s= "h"; }
    else if (s[0] == '>') { fn_name= "cmsy"; s= "i"; }
    else if (s[0] == '|') { fn_name= "cmsy"; s= "j"; }
    else if (s[0] == '\\') { fn_name= "cmsy"; s= "n"; }
    else if (s[0] == '{') { fn_name= "cmsy"; s= "f"; }
    else if (s[0] == '}') { fn_name= "cmsy"; s= "g"; }
  }
  int sz= script (env->fn_size, env->index_level);
  font gfn (tex_font (env->dis, fn_name, sz, (int) (env->magn*env->dpi)));
  box b= text_box (decorate (ip), 0, s, gfn, col);
  array<box> bs (1);
  bs[0]= b;
  a << line_item (STD_ITEM, composite_box (decorate (ip), bs), HYPH_INVALID);
}

void
concater_rep::flag_ok (string s, path ip, color col) {
  path dip = decorate_right (ip);
  SI h= 4*env->fn->wfn/5;
  int r, g, b;
  env->dis->get_rgb (col, r, g, b);
  r= 255- (255 - r)/6;
  g= 255- (255 - g)/6;
  b= 255- (255 - b)/6;
  color light= env->dis->rgb (r, g, b);
  string info_flag= env->get_string (INFO_FLAG);
  if (info_flag == "short") {
    box infob= info_box (dip, h, env->fn->wline, col, light);
    box specb= specific_box (ip, infob, PS_DEVICE_SCREEN, env->fn);
    print (STD_ITEM, specb);
  }
  if (info_flag == "detailed") {
    int sz= script (env->fn_size, env->index_level+2);
    font gfn (tex_font (env->dis, "ecrm", sz, (int) (env->magn*env->dpi)));
    box textb= text_box (decorate (ip), 0, s, gfn, col);
    box flagb= flag_box (dip, textb, h, env->fn->wline, col, light);
    box specb= specific_box (ip, flagb, PS_DEVICE_SCREEN, env->fn);
    print (STD_ITEM, specb);
  }
}

void
concater_rep::flag (string s, path ip, color col) {
  if (is_accessible (ip) && (!env->read_only))
    flag_ok (s, ip, col);
}

/******************************************************************************
* Printing spaces, setting penalties and limits
******************************************************************************/

void
concater_rep::print (space spc) {
  int n= N(a);
  if (n==0) return;
  a[n-1]->spc= max (spc, a[n-1]->spc);
}

void
concater_rep::penalty_min (int p) {
  if (N(a)>0) a[N(a)-1]->penalty = min (a[N(a)-1]->penalty, p);
}

void
concater_rep::penalty_max (int p) {
  if (N(a)>0) a[N(a)-1]->penalty = max (a[N(a)-1]->penalty, p);
}

void
concater_rep::with_limits (int status) {
  if (env->display_style || (status == LIMITS_ALWAYS))
    if (N(a)>0)
      a[N(a)-1]->limits = true;
}

/******************************************************************************
* Typesetting generic objects
******************************************************************************/

void
concater_rep::typeset (tree t, path ip) {
  // cout << "Typeset " << t << ", " << ip << ", " << obtain_ip (t) << "\n";

  /*
  if (obtain_ip (t) != ip)
    cout << "TeXmacs] Wrong ip: " << t << "\n"
	 << "       ] " << obtain_ip (t) << " -> " << ip << "\n";
  */

  if (!is_accessible (ip)) {
    path ip2= obtain_ip (t);
    if (ip2 != path (DETACHED))
      ip= ip2;
  }

  if (is_atomic (t)) {
    typeset_string (t->label, ip);
    return;
  }

  switch (L (t)) {
  case UNINIT:
  case ERROR:
    typeset_error (t, ip);
    break;
  case RAW_DATA:
    typeset_inactive (t, ip);
    break;
  case DOCUMENT:
    typeset_document (t, ip);
    break;
  case PARA:
    typeset_paragraph (t, ip);
    break;
  case SURROUND:
    typeset_surround (t, ip);
    break;
  case CONCAT:
    typeset_concat (t, ip);
    break;
  case GROUP:
    typeset_group (t, ip);
    break;
  case HIDDEN:
    (void) env->exec (t);
    break;
  case HSPACE:
    t= env->exec (t);
    typeset_hspace (t, ip);
    break;
  case VAR_VSPACE:
    flag (env->drd->get_name (L(t)), ip, env->dis->brown);
    t= tree (VAR_VSPACE, env->exec (tree (TMLEN, A(t))));
    control (t, ip);
    break;
  case VSPACE:
    flag (env->drd->get_name (L(t)), ip, env->dis->brown);
    t= tree (VSPACE, env->exec (tree (TMLEN, A(t))));
    control (t, ip);
    break;
  case SPACE:
    t= env->exec (t);
    typeset_space (attach_here (t, ip));
    break;
  case HTAB:
    print (space (env->as_length (t[0])));
    control (t, ip);
    break;
  case MOVE:
    typeset_move (t, ip);
    break;
  case RESIZE:
    typeset_resize (t, ip);
    break;
  case REPEAT:
    typeset_repeat (t, ip);
    break;
  case _FLOAT:
    typeset_float (t, ip);
    break;
  case DATOMS:
    typeset_formatting (t, ip, ATOM_DECORATIONS);
    break;
  case DLINES:
    typeset_formatting (t, ip, LINE_DECORATIONS);
    break;
  case DPAGES:
    typeset_formatting (t, ip, PAGE_DECORATIONS);
    break;
  case DBOX:
    typeset_decorated_box (t, ip);
    break;

  case WITH_LIMITS:
    with_limits (LIMITS_DISPLAY);
    flag ("with-limits", ip, env->dis->brown);
    control (t, ip);
    break;
  case LINE_BREAK:
    if (N(a)>0) a[N(a)-1]->penalty = 0;	
    flag ("line-break", ip, env->dis->brown);
    control (t, ip);
    break;
  case NEW_LINE:
  case NEXT_LINE:
    {
      string name= env->drd->get_name (L(t));
      flag (name, ip, env->dis->brown);
      control (t, ip);
      break;
    }
  case NO_BREAK:
    if (N(a)>0) a[N(a)-1]->penalty = HYPH_INVALID;
    if ((N(a)>1) &&
	(a[N(a)-1]->type == STRING_ITEM) &&
	(a[N(a)-1]->b->get_leaf_string () == ""))
      a[N(a)-2]->penalty = HYPH_INVALID;	
    flag ("no line break", ip, env->dis->brown);
    control (t, ip);
    break;
  case YES_INDENT:
    flag ("yes-first-indent", ip, env->dis->brown);
    control (tuple ("env_par", PAR_FIRST, env->read (PAR_FIRST)), ip);
    break;
  case NO_INDENT:
    flag ("no-first-indent", ip, env->dis->brown);
    control (tuple ("env_par", PAR_FIRST, "0cm"), ip);
    break;
  case VAR_YES_INDENT:
    flag ("yes-first-indent-after", ip, env->dis->brown);
    control (tuple ("env_par", PAR_NO_FIRST, "false"), ip);
    break;
  case VAR_NO_INDENT:
    flag ("no-first-indent-after", ip, env->dis->brown);
    control (tuple ("env_par", PAR_NO_FIRST, "true"), ip);
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
      flag (name, ip, env->dis->brown);
      control (t, ip);
      break;
    }

  case LEFT:
    typeset_large (t, ip, LEFT_BRACKET_ITEM, "<left-");
    break;
  case MID:
    typeset_large (t, ip, MIDDLE_BRACKET_ITEM, "<mid-");
    break;
  case RIGHT:
    typeset_large (t, ip, RIGHT_BRACKET_ITEM, "<right-");
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
    if ((N(t)>0) && is_table (t[N(t)-1])) typeset_table (t, ip);
    else typeset_formatting (t, ip, CELL_FORMAT);
    break;
  case TWITH:
  case CWITH:
  case TMARKER:
    typeset_inactive (t, ip);
    break;
  case TABLE:
    typeset_table (t, ip);
    break;
  case ROW:
  case CELL:
  case SUBTABLE:
    break;

  case ASSIGN:
    typeset_assign (t, ip);
    break;
  case WITH:
    typeset_with (t, ip);
    break;
  case PROVIDES:
    typeset_executable (t, ip);
    break;
  case QUOTE_VALUE:
    typeset_inactive (t, ip);
    break;
  case VALUE:
    typeset_value (t, ip);
    break;
  case MACRO:
    typeset_inactive (t, ip);
    break;
  case DRD_PROPS:
    typeset_drd_props (t, ip);
    break;
  case ARG:
    typeset_argument (t, ip);
    break;
  case QUOTE_ARG:
    typeset_inactive (t, ip);
    break;
  case COMPOUND:
    typeset_compound (t, ip);
    break;
  case XMACRO:
    typeset_inactive (t, ip);
    break;
  case GET_LABEL:
    typeset_executable (t, ip);
    break;
  case GET_ARITY:
    typeset_executable (t, ip);
    break;
  case MAP_ARGS:
    typeset_rewrite (t, ip);
    break;
  case EVAL_ARGS:
    typeset_eval_args (t, ip);
    break;
  case MARK:
    typeset_mark (t, ip);
    break;
  case EVAL:
    typeset_eval (t, ip);
    break;
  case QUOTE:
    typeset_inactive (t, ip);
    break;
  case QUASI:
    typeset_eval (tree (EVAL, tree (QUASIQUOTE, t[0])), ip);
    break;
  case QUASIQUOTE:
  case UNQUOTE:
  case VAR_UNQUOTE:
  case COPY:
    typeset_executable (t, ip);
    break;
  case IF:
    typeset_if (t, ip);
    break;
  case VAR_IF:
    typeset_var_if (t, ip);
    break;
  case CASE:
    typeset_case (t, ip);
    break;
  case WHILE:
  case FOR_EACH:
    typeset_executable (t, ip);
    break;
  case EXTERN:
    typeset_rewrite (t, ip);
    break;
  case INCLUDE:
    typeset_include (t, ip);
    break;
  case USE_PACKAGE:
    typeset_executable (t, ip);
    break;

  case OR:
  case XOR:
  case AND:
  case NOT:
  case PLUS:
  case MINUS:
  case TIMES:
  case OVER:
  case DIV:
  case MOD:
  case MERGE:
  case LENGTH:
  case RANGE:
  case NUMBER:
  case _DATE:
  case TRANSLATE:
  case CHANGE_CASE:
  case FIND_FILE:
  case IS_TUPLE:
  case LOOK_UP:
  case EQUAL:
  case UNEQUAL:
  case LESS:
  case LESSEQ:
  case GREATER:
  case GREATEREQ:
  case BOX_INFO:
  case FRAME_DIRECT:
  case FRAME_INVERSE:
    typeset_executable (t, ip);
    break;

  case CM_LENGTH:
  case MM_LENGTH:
  case IN_LENGTH:
  case PT_LENGTH:
  case BP_LENGTH:
  case DD_LENGTH:
  case PC_LENGTH:
  case CC_LENGTH:
  case FS_LENGTH:
  case FBS_LENGTH:
  case EM_LENGTH:
  case LN_LENGTH:
  case SEP_LENGTH:
  case YFRAC_LENGTH:
  case EX_LENGTH:
  case FN_LENGTH:
  case FNS_LENGTH:
  case BLS_LENGTH:
  case SPC_LENGTH:
  case XSPC_LENGTH:
  case PAR_LENGTH:
  case PAG_LENGTH:
  case TMPT_LENGTH:
  case PX_LENGTH:
    typeset_executable (t, ip);
    break;

  case STYLE_ONLY:
  case VAR_STYLE_ONLY:
  case ACTIVE:
  case VAR_ACTIVE:
  case INACTIVE:
  case VAR_INACTIVE:
    typeset_compound (t, ip);
    break;
  case REWRITE_INACTIVE:
    typeset_rewrite (t, ip);
    break;
  case INLINE_TAG:
  case OPEN_TAG:
  case MIDDLE_TAG:
  case CLOSE_TAG:
    typeset_src_tag (t, ip);
    break;
  case SYMBOL:
  case LATEX:
  case HYBRID:
  case TUPLE:
  case ATTR:
  case TMLEN:
  case COLLECTION:
  case ASSOCIATE:
  case BACKUP:
    typeset_inactive (t, ip);
    break;
  case LABEL:
    typeset_label (t, ip);
    break;
  case REFERENCE:
    typeset_reference (t, ip, 0);
    break;
  case PAGEREF:
    typeset_reference (t, ip, 1);
    break;
  case WRITE:
    typeset_write (t, ip);
    break;
  case SPECIFIC:
    typeset_specific (t, ip);
    break;
  case HLINK:
    typeset_hyperlink (t, ip);
    break;
  case ACTION:
    typeset_action (t, ip);
    break;
  case TAG:
    typeset_tag (t, ip);
    break;
  case MEANING:
    typeset_meaning (t, ip);
    break;
  case FLAG:
    typeset_flag (t, ip);
    break;

  case GRAPHICS:
    typeset_graphics (t, ip);
    break;
  case SUPERPOSE:
    typeset_superpose (t, ip);
    break;
  case TEXT_AT:
    typeset_text_at (t, ip);
    break;
  case _POINT:
    typeset_point (t, ip);
    break;
  case LINE:
    typeset_line (t, ip, false);
    break;
  case CLINE:
    typeset_line (t, ip, true);
    break;
  case ARC:
    typeset_arc (t, ip, false);
    break;
  case CARC:
    typeset_arc (t, ip, true);
    break;
  case SPLINE:
    typeset_spline (t, ip, false);
    break;
  case VAR_SPLINE:
    typeset_var_spline (t, ip);
    break;
  case CSPLINE:
    typeset_cspline (t, ip);
    break;
  case FILL:
    typeset_fill (t, ip);
    break;
  case POSTSCRIPT:
    typeset_postscript (t, ip);
    break;
  default:
    if (L(t) < START_EXTENSIONS) print (STD_ITEM, test_box (ip));
    else typeset_compound (t, ip);
    break;
  }
}

/******************************************************************************
* User interface
******************************************************************************/

concater_rep::concater_rep (edit_env env2): env (env2) {}

array<line_item>
typeset_concat (edit_env env, tree t, path ip) {
  concater ccc= new concater_rep (env);
  ccc->typeset (t, ip);
  ccc->finish ();
  array<line_item> a= ccc->a;
  delete ccc;
  return a;
}

array<line_item>
typeset_marker (edit_env env, path ip) {
  concater ccc= new concater_rep (env);
  ccc->marker (ip);
  array<line_item> a= ccc->a;
  delete ccc;
  return a;
}

box
typeset_as_concat (edit_env env, tree t, path ip) {
  concater ccc= new concater_rep (env);
  ccc->typeset (t, ip);
  ccc->finish ();
  array<line_item> a= ccc->a;

  int i, n=N(a);
  if (n == 0) return empty_box (ip); // FIXME: n=0 should never happen
  array<box> items (n);
  array<SI>  spc (n);
  if (n>0) {
    spc[0]=0;
    for (i=0; i<n-1; i++) {
      items[i]  = a[i]->b;
      spc  [i+1]= a[i]->spc->def;
    }
    items[i]= a[i]->b;
  }
  box b= concat_box (ip, items, spc);

  delete ccc;
  return b;
}

box
typeset_as_box (edit_env env, tree t, path ip) {
  box b= typeset_as_concat (env, t, ip);

  SI ox= 0;
  int i, n=N(b);
  for (i=0; i<n; i++)
    if (b[i]->w() != 0)
      ox= b[i]->x1;

  array<box> bs (1);
  array<SI>  xs (1);
  array<SI>  ys (1);
  bs[0]= b;
  xs[0]= ox;
  ys[0]= 0;
  return composite_box (ip, bs, xs, ys);
}

tree
box_info (edit_env env, tree t, string what) {
  box b= typeset_as_concat (env, attach_here (t, decorate ()));
  tree r= tuple();
  for (int i=0; i<N(what); i++) {
    switch (what[i]) {
    case 'l': r << as_string (b->x1); break;
    case 'b': r << as_string (b->y1); break;
    case 'r': r << as_string (b->x2); break;
    case 't': r << as_string (b->y2); break;
    case 'w': r << as_string (b->x2 - b->x1); break;
    case 'h': r << as_string (b->y2 - b->y1); break;
    case 'L': r << as_string (b->x3); break;
    case 'B': r << as_string (b->y3); break;
    case 'R': r << as_string (b->x4); break;
    case 'T': r << as_string (b->y4); break;
    case 'W': r << as_string (b->x4 - b->x3); break;
    case 'H': r << as_string (b->y4 - b->y3); break;
    case '.':
      if (N(r)==1) return as_string (r[0]) * "tmpt";
      else if (N(r)==0) return tree (ERROR, "No query for box-info");
      else return tree (ERROR, "More than one query for box-info");
    }
  }
  return r;
}
