
/******************************************************************************
* MODULE     : pager.cpp
* DESCRIPTION: Places typesetted paragraphs on pages
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef PAGER_H
#define PAGER_H
#include "typesetter.hpp"
#include "Format/line_item.hpp"
#include "Format/page_item.hpp"
#include "Format/stack_border.hpp"
#include "Page/skeleton.hpp"

class pager_rep {
public:
  path                 ip;
  edit_env             env;
  hashmap<string,tree> style;
  array<page_item>     l;

  bool         paper;
  int          quality;
  bool         show_hf;
  SI           text_width;
  SI           text_height;
  SI           width;
  SI           height;
  SI           odd;
  SI           even;
  SI           top;
  SI           bot;
  SI           may_extend;
  SI           may_shrink;
  SI           head_sep;
  SI           foot_sep;
  SI           col_sep;
  space        fn_sep;
  space        fnote_sep;
  SI           fnote_bl;
  space        float_sep;
  SI           mnote_sep;

  int          page_offset;
  SI           cur_top;
  array<box>   pages;

  array<box>   lines_bx;
  array<space> lines_ht;

protected: // making papyrus boxes
  array<page_item> pap_main;
  array<page_item> pap_fnote;

  void papyrus_fnote_float (array<page_item> from, array<page_item>& to);
  void papyrus_mcolumn (array<page_item>& l);
  void papyrus_make (array<page_item> l);

protected: // making page boxes
  box  pages_format (array<page_item> l, SI ht, SI tcor, SI bcor);
  box  pages_format (insertion ins);
  box  pages_format (pagelet pg);
  box  pages_make_page (pagelet pg);
  void pages_make ();
  void papyrus_make ();

public:
  pager_rep (path ip, edit_env env, array<page_item> l);

  //void start_page ();
  //void print (page_item item);
  //void end_page (bool flag);
  box  make_header (bool empty_flag);
  box  make_footer (bool empty_flag);
  box  make_pages ();

  friend struct stacker_rep;
  friend box typeset_paragraph (edit_env env, tree t, path ip);
  friend box typeset_document (edit_env env, tree t, path ip);
};

typedef pager_rep* pager;

#endif // defined PAGER_H
