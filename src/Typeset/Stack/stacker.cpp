
/******************************************************************************
* MODULE     : stacker.cpp
* DESCRIPTION: Vertical formatting of paragraphs
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "Stack/stacker.hpp"
#include "Boxes/construct.hpp"

/******************************************************************************
* Constructors and basic routines for stackers
******************************************************************************/

stacker_rep::stacker_rep ():
  l (0), unit_flag (false), unit_start (0) {}

void
stacker_rep::set_env_vars (
  SI height2, SI sep2, SI hor_sep2, SI ver_sep2, SI bot2, SI top2)
{
  sb->height = height2;
  sb->sep    = sep2;
  sb->hor_sep= hor_sep2;
  sb->ver_sep= ver_sep2;
  sb->bot    = bot2;
  sb->top    = top2;
}

/******************************************************************************
* Compute the degree of inshoveability between two successive lines
******************************************************************************/

#include "merge_sort.hpp"

static int
get_pos (array<SI> a, SI which) {
  int step, test;
  step= test= N(a)>>1;
  while (a[test] != which) {
    step= step >> 1;
    if (step==0) {
      if (which < a[test]) return test-1;
      else return test+1;
    }
    else {
      if (which < a[test]) test= max (0, test- step);
      else test= min (N(a)-1, test+ step);
    }
  }
  return test;
}

static SI
shove_in (box b1, box b2, stack_border sb) {
  SI  hor_sep= sb->hor_sep;
  SI  top    = sb->top;
  SI  bot    = sb->bot;

  // quick test whether there are boxes in b1 above boxes in b2
  int i, j;
  SI min1= PLUS_INFINITY, max1= MINUS_INFINITY;
  SI min2= PLUS_INFINITY, max2= MINUS_INFINITY;
  for (i=0; i<N(b1); i++)
    if (b1[i]->w() > 0) {
      if (b1->sx1(i) < min1) min1= b1->sx1(i);
      if (b1->sx2(i) > max1) max1= b1->sx2(i);
    }
  for (i=0; i<N(b2); i++)
    if (b2[i]->w() > 0) {
      if (b2->sx1(i) < min2) min2= b2->sx1(i);
      if (b2->sx2(i) > max2) max2= b2->sx2(i);
    }
  if ((max1 + hor_sep < min2) || (max2 + hor_sep < min1)) return 0;

  // longer determination of height after shoving
  array<SI> hpos;
  for (i=0; i<N(b1); i++)
    if (b1[i]->w() > 0)
      hpos << (b1->sx1(i)- hor_sep) << (b1->sx2(i)+ hor_sep);
  for (i=0; i<N(b2); i++) 
    if (b2[i]->w() > 0)
      hpos << (b2->sx1(i)- hor_sep) << (b2->sx2(i)+ hor_sep);
  merge_sort (hpos);  

  int n= N(hpos)-1;
  array<SI> vpos1 (n);
  array<SI> vpos2 (n);
  for (i=0; i<n; i++) {
    vpos1[i]= bot;
    vpos2[i]= top;
  }

  for (i=0; i<N(b1); i++) {
    SI  y    = b1->sy1(i);
    int start= get_pos (hpos, b1->sx1(i)- hor_sep);
    int end  = get_pos (hpos, b1->sx2(i)+ hor_sep);
    for (j=start; j<end; j++)
      vpos1[j]= min (vpos1[j], y);
  }
  for (i=0; i<N(b2); i++) {
    SI  y    = b2->sy2(i);
    int start= get_pos (hpos, b2->sx1(i)- hor_sep);
    int end  = get_pos (hpos, b2->sx2(i)+ hor_sep);
    for (j=start; j<end; j++)
      vpos2[j]= max (vpos2[j], y);
  }

  SI m= vpos2[0]-vpos1[0];
  for (i=1; i<n; i++)
    m= max (m, vpos2[i]-vpos1[i]);
  return m;
}

static void
shove (page_item& item1, page_item& item2, stack_border sb) {
  box b1= item1->b, b2= item2->b;
  while (true) {
    int type= b1->get_type ();
    if ((type == MOVE_BOX) && (b1->sx(0) == 0)) b1= b1[0];
    else if ((type == STACK_BOX) && (N(b1)>0)) b1= b1[N(b1)-1];
    else break;
  }
  while (true) {
    int type= b2->get_type ();
    if ((type == MOVE_BOX) && (b2->sx(0) == 0)) b2= b2[0];
    else if ((type == STACK_BOX) && (N(b2)>0)) b2= b2[0];
    else break;
  }

  if ((b2->y2- b1->y1) < (sb->height- max (sb->sep, sb->ver_sep)))
    // enough place
    item1->spc= item1->spc + space (sb->height- (b2->y2- b1->y1));
  else {
    SI sh= shove_in (b1, b2, sb);
    // cout << "Shove: " << sh/256 << "\n";
    if (sh == 0) {
      // no collisions
      SI h= max (sb->height, max (b2->y2, b2->y2 - b1->y1 - (sb->height>>1)));
      item1->spc= item1->spc + space (h- (b2->y2- b1->y1));
    }
    else {
      // collisions
      SI h= max (sb->height, sh + sb->ver_sep);
      item1->spc= item1->spc + space (h- (b2->y2- b1->y1));
    }
  }
}

/******************************************************************************
* Printing to a stacker
******************************************************************************/

void
stacker_rep::print (box b, array<lazy> fl, int nr_cols) {
  int i= N(l)-1;
  while ((i>=0) && (l[i]->type == PAGE_CONTROL_ITEM)) i--;
  l << page_item (b, fl, nr_cols);
  if ((!unit_flag) && (i>=0)) {
    l[i]= copy (l[i]);
    shove (l[i], l[N(l)-1], sb);
  }
  unit_flag= false;
}

void
stacker_rep::print (tree t, int nr_cols, bool before) {
  if (before) l << page_item (t, nr_cols);
  else unit_ctrl << page_item (t, nr_cols);
}

void
stacker_rep::print (space spc) {
  int i= N(l)-1;
  while ((i>=0) && (l[i]->type == PAGE_CONTROL_ITEM)) i--;
  if (i<0) return;
  l[i]->spc = l[i]->spc + spc;
}

/******************************************************************************
* Merging stacks
******************************************************************************/

void
merge_stack (array<page_item>& l, stack_border& sb,
	     array<page_item> l2, stack_border sb2)
{
  int i= N(l)-1;
  while ((i>=0) && (l[i]->type == PAGE_CONTROL_ITEM)) i--;
  if (i>=0) {
    l[i]= copy (l[i]);
    if (N(l2)>0) shove (l[i], l2[0], max (sb, sb2));
    l[i]->spc += max (sb->vspc_after, sb2->vspc_before);
    if (sb->nobr_after || sb2->nobr_before) l[i]->penalty= HYPH_INVALID;
  }
  else {
    sb->vspc_before= sb2->vspc_before;
    sb->nobr_before= sb2->nobr_before;
  }
  sb->vspc_after= sb2->vspc_after;
  sb->nobr_after= sb2->nobr_after;
  if (N(l2) > 0) {
    sb->height = sb2->height;
    sb->sep    = sb2->sep;
    sb->ver_sep= sb2->ver_sep;
    sb->hor_sep= sb2->hor_sep;
    sb->top    = sb2->top;
    sb->bot    = sb2->bot;
  }
  l << l2;
}

/******************************************************************************
* Other routines
******************************************************************************/

void
stacker_rep::new_paragraph () {
  if (unit_start == 0) {
    sb->vspc_before= unit_sb->vspc_before;
    sb->vspc_after = unit_sb->vspc_after;
    sb->nobr_before= unit_sb->nobr_before;
    sb->nobr_after = unit_sb->nobr_after;
  }
  else {
    int i= unit_start-1;
    while ((i>=0) && (l[i]->type == PAGE_CONTROL_ITEM)) i--;
    if (i<0) return;
    l[i]->spc= l[i]->spc + unit_sb->vspc_before;
    sb->vspc_after= unit_sb->vspc_after;
    if (unit_sb->nobr_before) l[i]->penalty= HYPH_INVALID;
    sb->nobr_after= unit_sb->nobr_after;
  }

  int i1= unit_start, i2= N(l)-1, mul= 100;
  while ((i1<=i2) && (mul>1)) {
    while ((i1<i2) && (l[i1]->type == PAGE_CONTROL_ITEM)) i1++;
    while ((i1<i2) && (l[i2]->type == PAGE_CONTROL_ITEM)) i2--;
    if (l[i1]->type != PAGE_CONTROL_ITEM)
      if (l[i1]->penalty < HYPH_INVALID)
	l[i1]->penalty *= mul;
    if (l[i2]->type != PAGE_CONTROL_ITEM)
      if (l[i2]->penalty < HYPH_INVALID)
	if (i1<i2) l[i2]->penalty *= mul;
    mul /= 10; i1++; i2--;
  }

  unit_sb->vspc_before= unit_sb->vspc_after;
  unit_sb->vspc_after = space (0);
  unit_sb->nobr_before= unit_sb->nobr_after;
  unit_sb->nobr_after = false;
  unit_start          = N(l);
}

void
stacker_rep::vspace_before (space spc) {
  unit_sb->vspc_before= max (unit_sb->vspc_before, spc);
}

void
stacker_rep::vspace_after (space spc) {
  unit_sb->vspc_after= max (unit_sb->vspc_after, spc);
}

void
stacker_rep::no_page_break_before () {
  unit_sb->nobr_before= true;
}

void
stacker_rep::no_page_break_after () {
  unit_sb->nobr_after= true;
}

void
stacker_rep::penalty (int pen) {
  int i= N(l)-1;
  while ((i>=0) && (l[i]->type == PAGE_CONTROL_ITEM)) i--;
  if (i<0) return;
  l[i]->penalty = pen;  
}

void
stacker_rep::flush () {
  l << unit_ctrl;
  unit_ctrl= array<page_item> (0);
}

/******************************************************************************
* User interface
******************************************************************************/

box
typeset_as_stack (edit_env env, tree t, path ip) {
  // cout << "Typeset as stack " << t << "\n";
  int i, n= N(t);
  stacker sss= new stacker_rep ();
  SI sep       = env->get_length (PAR_SEP);
  SI hor_sep   = env->get_length (PAR_HOR_SEP);
  SI ver_sep   = env->get_length (PAR_VER_SEP);
  SI height    = env->as_length (string ("1fn"))+ sep;
  SI bot       = 0;
  SI top       = env->fn->yx;
  sss->set_env_vars (height, sep, hor_sep, ver_sep, bot, top);
  for (i=0; i<n; i++)
    sss->print (typeset_as_concat (env, t[i], descend (ip, i)));

  n= N(sss->l);
  array<box> lines_bx (n);
  array<SI>  lines_ht (n);
  for (i=0; i<n; i++) {
    page_item item= copy (sss->l[i]);
    lines_bx[i]= item->b;
    lines_ht[i]= item->spc->def;
  }

  delete sss;
  box b= stack_box (ip, lines_bx, lines_ht);
  SI dy= n==0? 0: b[0]->y2;
  return move_box (ip, stack_box (ip, lines_bx, lines_ht), 0, dy);
}
