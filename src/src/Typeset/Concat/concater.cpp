
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
  a << line_item (STD_ITEM, b, HYPH_INVALID);
}

void
concater_rep::flag (string s, path ip, color col) {
  if (is_accessible (ip) && (!env->read_only)) {
    path dip = decorate_right (ip);
    SI h= 4*env->fn->wquad/5;
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

#define ACTIVATED (active_flag && (!env->preamble))

void
concater_rep::typeset (tree t, path ip, bool active_flag) {
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
  case DOCUMENT:
    if (ACTIVATED) typeset_document (t, ip);
    else typeset_inactive ("document", t, ip);
    break;
  case PARAGRAPH:
    if (ACTIVATED) typeset_paragraph (t, ip);
    else typeset_inactive ("paragraph", t, ip);
    break;
  case SURROUND:
    if (ACTIVATED) typeset_surround (t, ip);
    else typeset_inactive ("surround", t, ip);
    break;
  case CONCAT:
    typeset_concat (t, ip);
    break;
  case GROUP:
    typeset_group (t, ip);
    break;
  case HSPACE:
    if (ACTIVATED) {
      t= env->exec (t);
      typeset_hspace (t, ip);
    }
    else typeset_inactive ("hspace", t, ip);
    break;
  case VSPACE_BEFORE:
    if (ACTIVATED) {
      t= env->exec (t);
      control (t, ip);
    }
    else typeset_inactive ("vspace*", t, ip);
    break;
  case VSPACE_AFTER:
    if (ACTIVATED) {
      t= env->exec (t);
      control (t, ip);
    }
    else typeset_inactive ("vspace", t, ip);
    break;
  case SPACE:
    if (ACTIVATED) {
      t= env->exec (t);
      typeset_space (t, ip);
    }
    else typeset_inactive ("space", t, ip);
    break;
  case HTAB:
    if (ACTIVATED) {
      print (space (env->decode_length (t[0])));
      control (t, ip);
    }
    else typeset_inactive ("tab", t, ip);
    break;
  case MOVE:
    if (ACTIVATED) typeset_move (t, ip);
    else typeset_inactive ("move", t, ip);
    break;
  case RESIZE:
    if (ACTIVATED) typeset_resize (t, ip);
    else typeset_inactive ("resize", t, ip);
    break;
  case REPEAT:
    if (ACTIVATED) typeset_repeat (t, ip);
    else typeset_inactive ("repeat", t, ip);
    break;
  case _FLOAT:
    if (ACTIVATED) typeset_float (t, ip);
    else typeset_inactive ("float", t, ip);
    break;
  case DECORATE_ATOMS:
    if (ACTIVATED) typeset_formatting (t, ip, ATOM_DECORATIONS);
    else typeset_inactive ("decorate atoms", t, ip);
    break;
  case DECORATE_LINES:
    if (ACTIVATED) typeset_formatting (t, ip, LINE_DECORATIONS);
    else typeset_inactive ("decorate lines", t, ip);
    break;
  case DECORATE_PAGES:
    if (ACTIVATED) typeset_formatting (t, ip, PAGE_DECORATIONS);
    else typeset_inactive ("decorate pages", t, ip);
    break;
  case DECORATED_BOX:
    typeset_decorated_box (t, ip);
    break;

  case WITH_LIMITS:
    if (ACTIVATED) {
      with_limits (LIMITS_DISPLAY);
      flag ("with-limits", ip, env->dis->brown);
      control (t, ip);
    }
    else typeset_inactive_string ("<with-limits>", ip);
    break;
  case LINE_BREAK:
    if (ACTIVATED) {
      if (N(a)>0) a[N(a)-1]->penalty = 0;	
      flag ("line-break", ip, env->dis->brown);
      control (t, ip);
    }
    else typeset_inactive_string ("<line-break>", ip);
    break;
  case NEW_LINE:
  case LINE_SEP:
  case NEXT_LINE:
    {
      string name= replace (as_string (L(t)), "_", "-");
      if (ACTIVATED) {
	flag (name, ip, env->dis->brown);
	control (t, ip);
      }
      else typeset_inactive_string ("<" * name * ">", ip);
      break;
    }
  case NO_BREAK:
    if (ACTIVATED) {
      if (N(a)>0) a[N(a)-1]->penalty = HYPH_INVALID;
      if ((N(a)>1) &&
	  (a[N(a)-1]->type == STRING_ITEM) &&
	  (a[N(a)-1]->b->get_leaf_string () == ""))
	a[N(a)-2]->penalty = HYPH_INVALID;	
      flag ("no-break", ip, env->dis->brown);
      control (t, ip);
    }
    else typeset_inactive_string ("<no-break>", ip);
    break;
  case YES_FIRST_INDENT:
    if (ACTIVATED) {
      flag ("yes-first-indent", ip, env->dis->brown);
      control (tuple ("env_par", PAR_FIRST, env->read (PAR_FIRST)), ip);
    }
    else typeset_inactive_string ("<yes-first-indent>", ip);
    break;
  case NO_FIRST_INDENT:
    if (ACTIVATED) {
      flag ("no-first-indent", ip, env->dis->brown);
      control (tuple ("env_par", PAR_FIRST, "0cm"), ip);
    }
    else typeset_inactive_string ("<no-first-indent>", ip);
    break;
  case YES_FIRST_INDENT_AFTER:
    if (ACTIVATED) {
      flag ("yes-first-indent-after", ip, env->dis->brown);
      control (tuple ("env_par", PAR_NO_FIRST, "false"), ip);
    }
    else typeset_inactive_string ("<yes-first-indent-after>", ip);
    break;
  case NO_FIRST_INDENT_AFTER:
    if (ACTIVATED) {
      flag ("no-first-indent-after", ip, env->dis->brown);
      control (tuple ("env_par", PAR_NO_FIRST, "true"), ip);
    }
    else typeset_inactive_string ("<no-first-indent-after>", ip);
    break;
  case PAGE_BREAK_BEFORE:
  case PAGE_BREAK:
  case NO_PAGE_BREAK_BEFORE:
  case NO_PAGE_BREAK_AFTER:
  case NEW_PAGE_BEFORE:
  case NEW_PAGE:
  case NEW_DOUBLE_PAGE_BEFORE:
  case NEW_DOUBLE_PAGE:
    {
      string name= replace (as_string (L(t)), "_", "-");
      if (ACTIVATED) {
	flag (name, ip, env->dis->brown);
	control (t, ip);
      }
      else typeset_inactive_string ("<" * name * ">", ip);
      break;
    }

  case LEFT:
    typeset_left (t, ip);
    break;
  case MIDDLE:
    typeset_middle (t, ip);
    break;
  case RIGHT:
    typeset_right (t, ip);
    break;
  case BIG:
    typeset_bigop (t, ip);
    break;
  case LEFT_PRIME:
    typeset_lprime (t, ip);
    break;
  case RIGHT_PRIME:
    typeset_rprime (t, ip);
    break;
  case BELOW:
    typeset_below (t, ip);
    break;
  case ABOVE:
    typeset_above (t, ip);
    break;
  case LEFT_SUB:
  case LEFT_SUP:
    typeset_script (t, ip, false);
    break;
  case RIGHT_SUB:
  case RIGHT_SUP:
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
  case WIDE_UNDER:
    typeset_wide (t, ip, false);
    break;
  case NEG:
    typeset_neg (t, ip);
    break;
  case TREE:
    typeset_tree (t, ip);
    break;

  case TABLE_FORMAT:
    if (ACTIVATED) {
      if ((N(t)>0) && is_table (t[N(t)-1])) typeset_table (t, ip);
      else typeset_formatting (t, ip, CELL_FORMAT);
    }
    else typeset_inactive ("table-format", t, ip);
    break;
  case TABLE_WITH:
    typeset_inactive ("table-with", t, ip);
    break;
  case CELL_WITH:
    typeset_inactive ("cell-with", t, ip);
    break;
  case TABLE_MARKER:
    typeset_inactive ("table-marker", t, ip);
    break;
  case TABLE:
    typeset_table (t, ip);
    break;
  case ROW:
  case CELL:
  case SUB_TABLE:
    break;

  case ASSIGN:
    if (ACTIVATED) typeset_assign (t, ip);
    else typeset_inactive ("assign", t, ip, 1);
    break;
  case WITH:
    if (ACTIVATED) typeset_with (t, ip);
    else typeset_inactive ("with", t, ip, N(t)-1);
    break;
  case PROVIDES:
    if (ACTIVATED) typeset_executable (t, ip);
    else typeset_inactive ("provides", t, ip);
    break;
  case VALUE:
    if (ACTIVATED) typeset_value (t, ip);
    else typeset_inactive ("value", t, ip);
    break;
  case MACRO:
    typeset_inactive ("macro", t, ip, N(t)-1);
    break;
  case DRD_PROPS:
    if (ACTIVATED) typeset_drd_props (t, ip);
    else typeset_inactive ("drd-properties", t, ip, 1);
    break;
  case ARGUMENT:
    if (ACTIVATED) typeset_argument (t, ip);
    else typeset_inactive ("argument", t, ip);
    break;
  case COMPOUND:
    if (ACTIVATED) typeset_compound (t, ip);
    else typeset_inactive_compound (t, ip);
    break;
  case XMACRO:
    typeset_inactive ("xmacro", t, ip, 1);
    break;
  case GET_LABEL:
    if (ACTIVATED) typeset_executable (t, ip);
    else typeset_inactive ("tree-label", t, ip);
    break;
  case GET_ARITY:
    if (ACTIVATED) typeset_executable (t, ip);
    else typeset_inactive ("arity", t, ip);
    break;
  case MAP_ARGS:
    if (ACTIVATED) typeset_rewrite (t, ip);
    else typeset_inactive ("map-args", t, ip);
    break;
  case EVAL_ARGS:
    if (ACTIVATED) typeset_eval_args (t, ip);
    else typeset_inactive ("eval-args", t, ip);
    break;
  case EVAL:
    if (ACTIVATED) typeset_eval (t, ip);
    else typeset_inactive ("eval", t, ip);
    break;
  case QUOTE:
    typeset_inactive ("quote", t, ip);
    break;
  case DELAY:
    typeset_inactive ("delay", t, ip);
    break;
  case HOLD:
    if (ACTIVATED) typeset_executable (t, ip);
    else typeset_inactive ("hold", t, ip);
    break;
  case RELEASE:
    if (ACTIVATED) typeset_executable (t, ip);
    else typeset_inactive ("release", t, ip);
    break;
  case EXTERN:
    if (ACTIVATED) typeset_rewrite (t, ip);
    else typeset_inactive ("extern", t, ip);
    break;
  case INCLUDE:
    if (ACTIVATED) typeset_include (t, ip);
    else typeset_inactive ("include", t, ip);
    break;

  case OR:
    if (ACTIVATED) typeset_executable (t, ip);
    else typeset_inactive ("or", t, ip);
    break;
  case XOR:
    if (ACTIVATED) typeset_executable (t, ip);
    else typeset_inactive ("xor", t, ip);
    break;
  case AND:
    if (ACTIVATED) typeset_executable (t, ip);
    else typeset_inactive ("and", t, ip);
    break;
  case NOT:
    if (ACTIVATED) typeset_executable (t, ip);
    else typeset_inactive ("not", t, ip);
    break;
  case PLUS:
    if (ACTIVATED) typeset_executable (t, ip);
    else typeset_inactive ("+", t, ip);
    break;
  case MINUS:
    if (ACTIVATED) typeset_executable (t, ip);
    else typeset_inactive ("-", t, ip);
    break;
  case TIMES:
    if (ACTIVATED) typeset_executable (t, ip);
    else typeset_inactive ("*", t, ip);
    break;
  case OVER:
    if (ACTIVATED) typeset_executable (t, ip);
    else typeset_inactive ("/", t, ip);
    break;
  case DIVIDE:
    if (ACTIVATED) typeset_executable (t, ip);
    else typeset_inactive ("div", t, ip);
    break;
  case MODULO:
    if (ACTIVATED) typeset_executable (t, ip);
    else typeset_inactive ("mod", t, ip);
    break;
  case MERGE:
    if (ACTIVATED) typeset_executable (t, ip);
    else typeset_inactive ("merge", t, ip);
    break;
  case LENGTH:
    if (ACTIVATED) typeset_executable (t, ip);
    else typeset_inactive ("length", t, ip);
    break;
  case RANGE:
    if (ACTIVATED) typeset_executable (t, ip);
    else typeset_inactive ("range", t, ip);
    break;
  case NUMBER:
    if (ACTIVATED) typeset_executable (t, ip);
    else typeset_inactive ("number", t, ip);
    break;
  case _DATE:
    if (ACTIVATED) typeset_executable (t, ip);
    else typeset_inactive ("date", t, ip);
    break;
  case TRANSLATE:
    if (ACTIVATED) typeset_executable (t, ip);
    else typeset_inactive ("translate", t, ip);
    break;
  case FIND_FILE:
    if (ACTIVATED) typeset_executable (t, ip);
    else typeset_inactive ("find file", t, ip);
    break;
  case IS_TUPLE:
    if (ACTIVATED) typeset_executable (t, ip);
    else typeset_inactive ("is tuple", t, ip);
    break;
  case LOOK_UP:
    if (ACTIVATED) typeset_executable (t, ip);
    else typeset_inactive ("look up", t, ip);
    break;
  case EQUAL:
    if (ACTIVATED) typeset_executable (t, ip);
    else typeset_inactive ("equal", t, ip);
    break;
  case UNEQUAL:
    if (ACTIVATED) typeset_executable (t, ip);
    else typeset_inactive ("unequal", t, ip);
    break;
  case LESS:
    if (ACTIVATED) typeset_executable (t, ip);
    else typeset_inactive ("less", t, ip);
    break;
  case LESSEQ:
    if (ACTIVATED) typeset_executable (t, ip);
    else typeset_inactive ("less or equal", t, ip);
    break;
  case GREATER:
    if (ACTIVATED) typeset_executable (t, ip);
    else typeset_inactive ("greater", t, ip);
    break;
  case GREATEREQ:
    if (ACTIVATED) typeset_executable (t, ip);
    else typeset_inactive ("greater or equal", t, ip);
    break;
  case IF:
    if (ACTIVATED) typeset_if (t, ip);
    else typeset_inactive ("if", t, ip);
    break;
  case VAR_IF:
    if (ACTIVATED) typeset_var_if (t, ip);
    else typeset_inactive ("if*", t, ip);
    break;
  case CASE:
    if (ACTIVATED) typeset_case (t, ip);
    else typeset_inactive ("case", t, ip);
    break;
  case WHILE:
    if (ACTIVATED) typeset_executable (t, ip);
    else typeset_inactive ("while", t, ip);
    break;

  case INACTIVE:
    typeset_inactive (t, ip);
    break;
  case ACTIVE:
    typeset (t[0], descend (ip, 0));
    break;
  case VAR_INACTIVE:
    typeset_inactive (t, ip);
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
  case HYBRID:
    typeset_inactive_hybrid (t, ip);
    break;
  case TUPLE:
    typeset_inactive ("tuple", t, ip);
    break;
  case ATTR:
    typeset_inactive ("attr", t, ip);
    break;
  case COLLECTION:
    typeset_inactive ("collection", t, ip);
    break;
  case ASSOCIATE:
    typeset_inactive ("associate", t, ip);
    break;
  case BACKUP:
    typeset_inactive ("backup", t, ip);
    break;
  case LABEL:
    if (ACTIVATED) typeset_label (t, ip);
    else typeset_inactive ("label", t, ip);
    break;
  case REFERENCE:
    if (ACTIVATED) typeset_reference (t, ip, 0);
    else typeset_inactive ("reference", t, ip);
    break;
  case PAGEREF:
    if (ACTIVATED) typeset_reference (t, ip, 1);
    else typeset_inactive ("page reference", t, ip);
    break;
  case WRITE:
    if (ACTIVATED) typeset_write (t, ip);
    else typeset_inactive ("write", t, ip);
    break;
  case SPECIFIC:
    if (ACTIVATED) typeset_specific (t, ip);
    else typeset_inactive_specific (t, ip);
    break;
  case HYPERLINK:
    if (ACTIVATED) typeset_hyperlink (t, ip);
    else typeset_inactive_action ("hyperlink", t, ip);
    break;
  case ACTION:
    if (ACTIVATED) typeset_action (t, ip);
    else typeset_inactive_action ("action", t, ip);
    break;
  case TAG:
    if (ACTIVATED) typeset_tag (t, ip);
    else typeset_inactive ("tag", t, ip);
    break;
  case MEANING:
    if (ACTIVATED) typeset_meaning (t, ip);
    else typeset_inactive ("meaning", t, ip);
    break;
  case FLAG:
    if (ACTIVATED) typeset_flag (t, ip);
    else typeset_inactive ("flag", t, ip);
    break;

  case GRAPHICS:
    if (ACTIVATED) typeset_graphics (t, ip);
    else typeset_inactive ("graphics", t, ip);
    break;
  case SUPERPOSE:
    if (ACTIVATED) typeset_superpose (t, ip);
    else typeset_inactive ("superpose", t, ip);
    break;
  case TEXT_AT:
    if (ACTIVATED) typeset_text_at (t, ip);
    else typeset_inactive ("text at", t, ip);
    break;
  case _POINT:
    if (ACTIVATED) typeset_graphics (t, ip);
    else typeset_inactive ("point", t, ip);
    break;
  case LINE:
    if (ACTIVATED) typeset_line (t, ip, false);
    else typeset_inactive ("line", t, ip);
    break;
  case CLINE:
    if (ACTIVATED) typeset_line (t, ip, true);
    else typeset_inactive ("cline", t, ip);
    break;
  case SPLINE:
    if (ACTIVATED) typeset_spline (t, ip);
    else typeset_inactive ("spline", t, ip);
    break;
  case VAR_SPLINE:
    if (ACTIVATED) typeset_var_spline (t, ip);
    else typeset_inactive ("var_spline", t, ip);
    break;
  case CSPLINE:
    if (ACTIVATED) typeset_cspline (t, ip);
    else typeset_inactive ("cspline", t, ip);
    break;
  case FILL:
    if (ACTIVATED) typeset_fill (t, ip);
    else typeset_inactive ("fill", t, ip);
    break;
  case POSTSCRIPT:
    if (ACTIVATED) typeset_postscript (t, ip);
    else typeset_inactive ("postscript", t, ip);
    break;
  default:
    if (L(t) < START_EXTENSIONS) print (STD_ITEM, test_box (ip));
    else {
      if (ACTIVATED) typeset_compound (t, ip);
      else typeset_inactive (as_string (L(t)), t, ip);
    }
    break;
  }
}

#undef ACTIVATED

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
