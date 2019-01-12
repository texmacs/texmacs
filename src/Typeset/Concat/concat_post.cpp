
/******************************************************************************
* MODULE     : concat_post.cpp
* DESCRIPTION: Second pass for typesetting paragraphs
*                - The real heights of brackets are determined
*                - scripts are glued
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "concater.hpp"
#include "analyze.hpp"
SI italic_correction (box, box);

/******************************************************************************
* Miscellaneous routines
******************************************************************************/

int
concater_rep::prec (int i) {
  do i--; while ((i>=0) && (a[i]->type==OBSOLETE_ITEM ||
                            a[i]->type==MARKER_ITEM));
  return i;
}

int
concater_rep::succ (int i) {
  do i++; while ((i<N(a)) && (a[i]->type==OBSOLETE_ITEM ||
                              a[i]->type==MARKER_ITEM));
  return i;
}

/******************************************************************************
* Gluing scripts
******************************************************************************/

void
concater_rep::pre_glue () {
  int i=0;
  while (true) {
    int j= succ(i);
    if (j >= N(a)) break;
    line_item item1= a[i];
    line_item item2= a[j];
    int t1= item1->type;
    int t2= item2->type;
    if (((t1 == RSUB_ITEM) && (t2 == RSUP_ITEM)) ||
	((t1 == RSUP_ITEM) && (t2 == RSUB_ITEM)) ||
	((t1 == LSUB_ITEM) && (t2 == LSUP_ITEM)) ||
	((t1 == LSUP_ITEM) && (t2 == LSUB_ITEM)))
      {
	bool  flag1 = (t1 == LSUB_ITEM) || (t1 == RSUB_ITEM);
	bool  flag2 = (t1 == LSUB_ITEM) || (t1 == LSUP_ITEM);
	int   type  = flag2? GLUE_LSUBS_ITEM: GLUE_RSUBS_ITEM;
	box   b1    = flag1? item1->b[0]: item2->b[0];
	box   b2    = flag1? item2->b[0]: item1->b[0];
	box   b     = script_box (b1->ip, b1, b2, env->fn);
	int   pen   = item2->penalty;
	space spc   = max (item1->spc, item2->spc);

	a[i]= line_item (type, OP_SKIP, b, pen);
	a[i]->spc = spc;
        for (int k=i+1; k<j; k++)
          if (a[k]->type == MARKER_ITEM)
            a[k]= line_item (OBSOLETE_ITEM, OP_SKIP, a[k]->b, a[k]->penalty);
	a[j]= line_item (OBSOLETE_ITEM, OP_SKIP, item2->b, pen);
      }
    i++;
  }
}

box
concater_rep::glue_left_markers (box b, int ref, int arg) {
  int i= arg+1;
  while (i < ref && a[i]->type == OBSOLETE_ITEM) i++;
  if (i >= ref) return b;
  array<box> bs;
  array<SI>  spc;
  while (i < ref) {
    if (a[i]->type == MARKER_ITEM) {
      bs  << a[i]->b;
      spc << 0;
      a[i]->type= OBSOLETE_ITEM;
    }
    i++;
  }
  bs  << b;
  spc << 0;
  return concat_box (b->ip, bs, spc);
}

box
concater_rep::glue_right_markers (box b, int ref, int arg, bool flag) {
  int i= ref+1;
  while (i < arg && a[i]->type == OBSOLETE_ITEM) i++;
  if (i >= arg) return b;
  array<box> bs;
  array<SI>  spc;
  if (flag) {
    for (int j=0; j<N(b); j++) {
      bs  << b[j];
      spc << 0;
    }
  }
  else {
    bs  << b;
    spc << 0;
  }
  while (i < arg) {
    if (a[i]->type == MARKER_ITEM) {
      bs  << a[i]->b;
      spc << 0;
      a[i]->type= OBSOLETE_ITEM;
    }
    i++;
  }
  return concat_box (b->ip, bs, spc);
}

void
concater_rep::glue (box b, int ref, int arg) {
  if (a[ref]->op_type == OP_BIG && arg >= ref && !a[ref]->limits)
    if (env->fn->math_type != MATH_TYPE_NORMAL)
      if (a[ref]->spc->def > 0) {
        space spc= env->fn->spc;
        a[ref]->spc += space (spc->min/3, spc->def/3, spc->def/3);
      }
  
  space spc = max (a[ref]->spc, a[arg]->spc);

  a[arg]  = line_item (OBSOLETE_ITEM, OP_SKIP, a[arg]->b, a[arg]->penalty);
  a[ref]  = line_item (arg<ref? GLUE_LEFT_ITEM: GLUE_RIGHT_ITEM,
                       a[ref]->op_type, b,
		       min (a[ref]->penalty, a[arg]->penalty));
  a[ref]->spc = spc;
}

void
concater_rep::glue (box b, int ref, int arg1, int arg2) {
  if (a[ref]->op_type == OP_BIG && !a[ref]->limits)
    if (env->fn->math_type != MATH_TYPE_NORMAL)
      if (a[ref]->spc->def > 0) {
        space spc= env->fn->spc;
        a[ref]->spc += space (spc->min/3, spc->def/3, spc->def/3);
      }

  space spc = max (a[ref]->spc, max (a[arg1]->spc, a[arg2]->spc));
  int   pen = min (a[ref]->penalty, min (a[arg1]->penalty, a[arg2]->penalty));

  space ref_spc= a[ref]->spc;
  a[arg1]= line_item (OBSOLETE_ITEM, OP_SKIP, a[arg1]->b, a[arg1]->penalty);
  a[arg2]= line_item (OBSOLETE_ITEM, OP_SKIP, a[arg2]->b, a[arg2]->penalty);
  a[ref]= line_item (GLUE_BOTH_ITEM, a[ref]->op_type, b, pen);
  a[ref]->spc = spc;
}

void
concater_rep::handle_scripts (int start, int end) {
  int i;
  for (i=start; i<=end; ) {
    if ((a[i]->type == OBSOLETE_ITEM) ||
	(a[i]->type == LSUB_ITEM) ||
	(a[i]->type == LSUP_ITEM) ||
	(a[i]->type == GLUE_LSUBS_ITEM) ||
	(a[i]->type == RSUB_ITEM) ||
	(a[i]->type == RSUP_ITEM) ||
	(a[i]->type == GLUE_RSUBS_ITEM)) { i++; continue; }

    path sip;
    int l= prec (i);
    box lb1, lb2;
    if (l < start) l= -1;
    else switch (a[l]->type) {
    case LSUB_ITEM:
      lb1= a[l]->b[0];
      sip= lb1->ip;
      break;
    case LSUP_ITEM:
      lb2= a[l]->b[0];
      sip= lb2->ip;
      break;
    case GLUE_LSUBS_ITEM:
      lb1= a[l]->b[0];
      lb2= a[l]->b[1];
      sip= lb2->ip;
      break;
    default:
      l = -1;
    }

    int r= succ (i);
    box rb1, rb2;
    if (r > end) r= N(a);
    else switch (a[r]->type) {
    case RSUB_ITEM:
      rb1= a[r]->b[0];
      sip= rb1->ip;
      break;
    case RSUP_ITEM:
      rb2= a[r]->b[0];
      sip= rb2->ip;
      break;
    case GLUE_RSUBS_ITEM:
      rb1= a[r]->b[0];
      rb2= a[r]->b[1];
      sip= rb2->ip;
      break;
    default:
      r = N(a);
    }

    box b;
    if (l==-1) {
      if (r==N(a)) { i++; continue; }
      else {
        box mb= glue_right_markers (a[i]->b, i, r, false);
        if (a[i]->limits)
          b= limit_box (sip, mb, rb1, rb2, env->fn, true);
        else
          b= right_script_box (sip, mb, rb1, rb2, env->fn, env->vert_pos);
        glue (b, i, r);
      }
    }
    else {
      box mb= glue_left_markers (a[i]->b, i, l);
      if (r==N(a)) {
        b= left_script_box (sip, mb, lb1, lb2, env->fn, env->vert_pos);
        glue (b, i, l);
      }
      else {
        mb= glue_right_markers (mb, i, r, true);
        b = side_box (sip, mb, lb1, lb2, rb1, rb2, env->fn, env->vert_pos);
        glue (b, i, l, r);
      }
    }
  }
}

void
concater_rep::clean_and_correct () {
  array<line_item> new_a;
  int i, prev=-1;
  for (i=0; i<N(a); i++)
    if (a[i]->type!=OBSOLETE_ITEM) {
      if (a[i]->b->w () != 0) {
	if (prev != -1) {
          SI cor= ::italic_correction (a[prev]->b, a[i]->b);
          if (cor != 0) {
            if (a[prev]->b->right_correction () != 0)
              a[prev]->spc += space (cor);
            else a[i-1]->spc += space (cor);
          }
        }
	prev= i;
      }
      new_a << a[i];
    }
  a= new_a;
}

/******************************************************************************
* Resize brackets
******************************************************************************/

void
concater_rep::handle_matching (int start, int end) {
  //cout << "matching " << start << " -- " << end << "\n";
  //cout << a << "\n\n";
  int i;
  SI y1=  MAX_SI;
  SI y2= -MAX_SI;
  bool uninit= true;
  a[start]->penalty++;
  a[end]->penalty++;
  for (i=start+1; i<end; i++) {
    if (a[i]->type == OBSOLETE_ITEM) continue;
    // cout << "  " << a[i] << ": " << (a[i]->b->y2- a[i]->b->y1) << "\n";
    // y1= min (y1, a[i]->b->sub_base());
    // y2= max (y2, a[i]->b->sup_base());
    SI lo, hi;
    a[i]->b->get_bracket_extents (lo, hi);
    y1= min (y1, lo);
    y2= max (y2, hi);
    a[i]->penalty++;
    uninit= false;
  }
  if (uninit) {
    y1= min (a[start]->b->y1, a[end]->b->y2);
    y2= max (a[start]->b->y1, a[end]->b->y2);
  }

  for (i=start; i<=end; i++) {
    int tp= a[i]->type;
    if (tp == LEFT_BRACKET_ITEM ||
	tp == MIDDLE_BRACKET_ITEM ||
	tp == RIGHT_BRACKET_ITEM)
      {
        string ls= a[i]->b->get_leaf_string ();
        pencil lp= a[i]->b->get_leaf_pencil ();
	font   fn= a[i]->b->get_leaf_font ();

        // find the middle of the bracket, around where to center
	SI mid= (a[i]->b->y1 + a[i]->b->y2) >> 1;
        bool custom=
          N(ls) > 2 && ls[N(ls)-2] <= '9' && ls[N(ls)-2] >= '0' &&
          !ends (ls, "-0>");
        if (custom) {
          int pos= N(ls)-1;
          while (pos > 0 && ls[pos] != '-') pos--;
          if (pos > 0 && ls[pos-1] == '-') pos--;
          string ss= ls (0, pos) * ">";
          box auxb= text_box (a[i]->b->ip, 0, ss, fn, lp);
          mid= (auxb->y1 + auxb->y2) >> 1;
        }

	// make symmetric and prevent from too large delimiters if possible
	SI Y1   = y1 + (fn->sep >> 1);
	SI Y2   = y2 - (fn->sep >> 1);
	SI tol  = fn->sep << 1;
	SI drift= ((Y1 + Y2) >> 1) - mid; // fn->yfrac;
	if (drift < 0) Y2 += min (-drift, tol) << 1;
	else Y1 -= min (drift, tol) << 1;

        // further adjustments when the enclosed expression is not very high
        // and for empty brackets
        SI h= y2 - y1 - fn->sep;
        SI d= 5 * fn->yx - h;
        if (d > 0) { Y1 += d/12; Y2 -= d/12; }
        if (N(ls) >= 8 && (ls[6] == '.' || ls[7] == '.'))
          if (starts (ls, "<left-.") || starts (ls, "<right-.")) {
            Y1 += d/6; Y2 -= d/12; }

        // replace item by large or small delimiter
        if (Y1 < fn->y1 || Y2 > fn->y2 || custom || use_poor_rubber (fn))
          a[i]->b= delimiter_box (a[i]->b->ip, ls, fn, lp, Y1, Y2, mid, y1, y2);
        else {
          string s= "<nobracket>";
          int j;
          for (j=0; j<N(ls); j++)
            if (ls[j] == '-') break;
          if (j<N(ls) && ls[N(ls)-1] == '>') s= ls (j+1, N(ls)-1);
          if (N(s) > 1 && s[0] != '<') s= "<" * s * ">";
          else if (N(s) == 0 || s == ".") s= "<nobracket>";
          a[i]->b= text_box (a[i]->b->ip, 0, s, fn, lp);
          tp= STD_ITEM;
        }
        a[i]->type= STD_ITEM;
      }
    if (tp == LEFT_BRACKET_ITEM)
      for (int j= i-1; j>=0; j--) {
	if (a[j]->type == MARKER_ITEM) {
	  SI Y1= a[i]->b->y1;
	  SI Y2= a[i]->b->y2;
	  //a[j]->b = marker_box (a[j]->b->find_lip (), 0, Y1, 0, Y2, a[j]->b);
	  a[j]->b   = marker_box (a[j]->b->find_lip (), 0, Y1, 0, Y2, a[i]->b);
	  a[j]->type= STD_ITEM;
	}
	else if (a[j]->type != CONTROL_ITEM) break;
      }
    if (tp == RIGHT_BRACKET_ITEM)
      for (int j= i+1; j<N(a); j++) {
	if (a[j]->type == MARKER_ITEM) {
	  SI Y1= a[i]->b->y1;
	  SI Y2= a[i]->b->y2;
	  //a[j]->b = marker_box (a[j]->b->find_lip (), 0, Y1, 0, Y2, a[j]->b);
	  a[j]->b   = marker_box (a[j]->b->find_lip (), 0, Y1, 0, Y2, a[i]->b);
	  a[j]->type= STD_ITEM;
	}
	else if (a[j]->type != CONTROL_ITEM) break;
      }
  }
}

void
concater_rep::handle_brackets () {
  int first=-1, start=0, i=0;
  while (i<N(a)) {
    if (a[i]->type==LEFT_BRACKET_ITEM) {
      if (first==-1) first= i;
      start= i;
    }
    if (a[i]->type==RIGHT_BRACKET_ITEM) {
      handle_scripts  (succ (start), prec (i));
      handle_matching (start, i);
      if (first!=-1) i=first-1;
      start= 0;
      first= -1;
    }
    i++;
  }
  if (N(a)>0) {
    handle_scripts  (0, N(a)-1);
    handle_matching (0, N(a)-1);
  }
}

/******************************************************************************
* Kill invalid spaces
******************************************************************************/

void
concater_rep::kill_spaces () {
  int i;
  for (i=N(a)-1; (i>0) && (a[i]->type == CONTROL_ITEM); i--)
    a[i-1]->spc= space (0);
  for (i=0; (i<N(a)) && (a[i]->type == CONTROL_ITEM); i++)
    a[i]->spc= space (0);

  for (i=0; i<N(a); i++)
    if (a[i]->type==CONTROL_ITEM) {
      if (is_formatting (a[i]->t)) {
	tree_label lab= L(a[i]->t);
	if ((lab==NEXT_LINE) || (lab==LINE_BREAK) || (lab==NEW_LINE))
	  {
	    if (i>0) a[i-1]->spc= space (0);
	    a[i]->spc= space (0);
	  }
      }

      if (is_tuple (a[i]->t, "env_par") ||
	  is_tuple (a[i]->t, "env_page"))
	a[i]->spc= space (0);
    }
}

/******************************************************************************
* Main control
******************************************************************************/

void
concater_rep::finish () {
  kill_spaces ();
  pre_glue ();
  handle_brackets ();
  clean_and_correct ();
}
