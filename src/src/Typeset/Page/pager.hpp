
/******************************************************************************
* MODULE     : pager.cpp
* DESCRIPTION: Places typesetted paragraphs on pages
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
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
  pager_rep (edit_env env, array<page_item> l);

  //void start_page ();
  //void print (page_item item);
  //void end_page (bool flag);
  box  make_header ();
  box  make_footer ();
  box  make_pages ();

  friend class stacker_rep;
  friend box typeset_paragraph (edit_env env, tree t, path ip);
  friend box typeset_document (edit_env env, tree t, path ip);
};

typedef pager_rep* pager;

#endif // defined PAGER_H
