
/******************************************************************************
* MODULE     : pager.cpp
* DESCRIPTION: Control routines for typesetting paragraphs
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "pager.hpp"
#include "Boxes/construct.hpp"

/******************************************************************************
* Routines for the pager class
******************************************************************************/

pager_rep::pager_rep (path ip2, edit_env env2, array<page_item> l2):
  ip (ip2), env (env2), style (UNINIT), l (l2)
{
  style (PAGE_THE_PAGE)   = tree (MACRO, compound ("page-nr"));
  style (PAGE_ODD_HEADER) = env->read (PAGE_ODD_HEADER);
  style (PAGE_ODD_FOOTER) = env->read (PAGE_ODD_FOOTER);
  style (PAGE_EVEN_HEADER)= env->read (PAGE_EVEN_HEADER);
  style (PAGE_EVEN_FOOTER)= env->read (PAGE_EVEN_FOOTER);
  style (PAGE_THIS_HEADER)= "";
  style (PAGE_THIS_FOOTER)= "";

  int nr_cols= env->get_int (PAR_COLUMNS);
  paper= (env->get_string (PAGE_MEDIUM) == "paper");
  string pbr= env->get_string (PAGE_BREAKING);
  quality= (pbr == "sloppy"? 0: (pbr == "medium"? 1: 2));
  env->get_page_pars (text_width, text_height, width, height,
		      odd, even, top, bot);
  may_extend= env->get_length (PAGE_EXTEND);
  may_shrink= env->get_length (PAGE_SHRINK);
  head_sep  = env->get_length (PAGE_HEAD_SEP);
  foot_sep  = env->get_length (PAGE_FOOT_SEP);
  col_sep   = env->get_length (PAR_COLUMNS_SEP);
  fn_sep    = env->get_space (PAR_FNOTE_SEP);
  fnote_sep = env->get_space (PAGE_FNOTE_SEP) + (2*env->fn->sep);
  fnote_bl  = env->get_length (PAGE_FNOTE_BARLEN);
  float_sep = env->get_space (PAGE_FLOAT_SEP);
  mnote_sep = env->get_length (PAGE_MNOTE_SEP);
  show_hf   = env->get_bool (PAGE_SHOW_HF) && paper;
  if (nr_cols > 1) text_width = (text_width+col_sep+1) * nr_cols - col_sep;

  page_offset= 0;
  cur_top= 0;
}

/******************************************************************************
* Subroutines
******************************************************************************/

box
format_stack (path ip, array<box> bx, array<space> ht) {
  int i, n= N(bx);
  array<SI> spc (n);
  for (i=0; i<n-1; i++) spc[i]= ht[i]->def;
  return stack_box (ip, bx, spc);  
}

box
format_stack (path ip, array<box> bx, array<space> ht, SI height,
	      bool may_stretch)
{
  int i, n= N(bx);
  array<SI> spc (n);
  space total (0);
  for (i=0; i<n-1; i++) total += space (bx[i]->h()) + ht[i];
  total += space (bx[i]->h());

  // stretching case
  if (may_stretch && (total->def < height) && (total->max > total->def)) {
    double f=
      ((double) (height - total->def)) /
      ((double) (total->max - total->def));
    for (i=0; i<n-1; i++)
      spc[i]= ht[i]->def+
	((SI) (f*((double) ht[i]->max- ht[i]->def)));
  }

  // shrinking case
  else if ((total->def > height) && (total->def > total->min)) {
    double f=
      ((double) (total->def - height)) /
      ((double) (total->def - total->min));
    if (f>1.0) f=1.0;
    for (i=0; i<n-1; i++)
      spc[i]= ht[i]->def-
	((SI) (f*((double) ht[i]->def- ht[i]->min)));
  }

  // normal case
  else for (i=0; i<n-1; i++) spc[i]= ht[i]->def;

  return stack_box (ip, bx, spc);
}

box
format_stack (path ip, array<page_item> l) {
  int i, n= N(l);
  array<box> bs  (n);
  array<SI>  spc (n);
  for (i=0; i<n-1; i++) {
    bs [i]= l[i]->b;
    spc[i]= l[i]->spc->def;
  }
  if (i<n) bs [i]= l[i]->b;
  return stack_box (ip, bs, spc);  
}

box
format_stack (path ip, array<page_item> l, SI height, bool may_stretch) {
  int i, n= N(l);
  array<box>   bs  (n);
  array<space> spc (n);
  for (i=0; i<n-1; i++) {
    bs [i]= l[i]->b;
    spc[i]= l[i]->spc;
  }
  if (i<n) bs [i]= l[i]->b;
  return format_stack (ip, bs, spc, height, may_stretch);
}

box
page_box (path ip, box b, tree page,
	  SI width, SI height, SI left, SI top, SI bot,
	  box header, box footer, SI head_sep, SI foot_sep)
{
  SI h_y= -top- header->y1+ head_sep;
  SI f_y= -bot- footer->y2- foot_sep;

  array<box> bs     (1); bs     [0]= b;
  array<SI>  bs_x   (1); bs_x   [0]= left;
  array<SI>  bs_y   (1); bs_y   [0]= -top;
  array<box> decs   (2); decs   [0]= header; decs   [1]= footer;
  array<SI>  decs_x (2); decs_x [0]= left  ; decs_x [1]= left;
  array<SI>  decs_y (2); decs_y [0]= h_y   ; decs_y [1]= f_y;

  return page_box (ip, page, width, height,
		   bs, bs_x, bs_y,
		   decs, decs_x, decs_y);
}

/******************************************************************************
* Typesetting a page
******************************************************************************/

/*
void
pager_rep::start_page () {
  lines_bx= array<box> (0);
  lines_ht= array<space> (0);
}

void
pager_rep::print (page_item item) {
  if (item->type == PAGE_CONTROL_ITEM) {
    if (is_tuple (item->t, "env_page")) {
      if (((item->t[1] == PAGE_THIS_HEADER) ||
	   (item->t[1] == PAGE_THIS_FOOTER)) &&
	  (item->t[2] == "")) style (item->t[1]->label)= " ";
      else if (item->t[1] == PAGE_NR)
	page_offset= as_int (item->t[2]->label)- N(pages)- 1;
      else style (item->t[1]->label)= copy (item->t[2]);
    }
    return;
  }
  lines_bx << item->b;
  lines_ht << item->spc;
}

void
pager_rep::end_page (bool flag) {
  box sb  = format_stack (ip, lines_bx, lines_ht, text_height, !flag);
  box lb  = move_box (ip, sb, 0, 0);
  SI  left= (N(pages)&1)==0? odd: even;
  box pb  = page_box (ip, lb, as_string (N(pages)+1+page_offset),
		      width, height, left, top, top+ text_height,
		      make_header(), make_footer(), head_sep, foot_sep);

  cur_top -= height;
  pages << pb;
}
*/

box
pager_rep::make_header () {
  if (!show_hf) return empty_box (decorate ());
  env->write (PAGE_NR, as_string (N(pages)+1+page_offset));
  env->write (PAGE_THE_PAGE, style[PAGE_THE_PAGE]);
  tree old= env->local_begin (PAR_COLUMNS, "1");
  string which= (N(pages)&1)==0? PAGE_ODD_HEADER: PAGE_EVEN_HEADER;
  if (style [PAGE_THIS_HEADER] != "") which= PAGE_THIS_HEADER;
  box b= typeset_as_concat (env, attach_here (tree (PARA, style[which]),
					      decorate()));
  style (PAGE_THIS_HEADER) = "";
  env->local_end (PAR_COLUMNS, old);
  return b;
}

box
pager_rep::make_footer () {
  if (!show_hf) return empty_box (decorate ());
  env->write (PAGE_NR, as_string (N(pages)+1+page_offset));
  env->write (PAGE_THE_PAGE, style[PAGE_THE_PAGE]);
  tree old= env->local_begin (PAR_COLUMNS, "1");
  string which= (N(pages)&1)==0? PAGE_ODD_FOOTER: PAGE_EVEN_FOOTER;
  if (style [PAGE_THIS_FOOTER] != "") which= PAGE_THIS_FOOTER;
  box b= typeset_as_concat (env, attach_here (tree (PARA, style[which]),
					      decorate()));
  style (PAGE_THIS_FOOTER) = "";
  env->local_end (PAR_COLUMNS, old);
  return b;
}

/******************************************************************************
* Typesetting all pages
******************************************************************************/

box
pager_rep::make_pages () {
  if (paper) pages_make ();
  else papyrus_make ();
  int i, nr_pages= N(pages);
  array<SI>  x  (nr_pages);
  array<SI>  y  (nr_pages);
  for (i=0; i<nr_pages; i++) {
    x[i]= 0;
    y[i]= (i==0? 0: y[i-1]- pages[i-1]->h());
  }
  return move_box (ip, scatter_box (ip, pages, x, y), 0, 0);
}
