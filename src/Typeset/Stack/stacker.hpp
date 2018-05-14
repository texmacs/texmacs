
/******************************************************************************
* MODULE     : stacker.hpp
* DESCRIPTION: Vertical formatting of paragraphs
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef STACKER_H
#define STACKER_H
#include "formatter.hpp"
#include "typesetter.hpp"
#include "Format/stack_border.hpp"
#include "Format/page_item.hpp"
#include "Format/line_item.hpp"

struct stacker_rep {
  path              ip;      // source location of the stacker
  array<page_item>  l;       // the vertically formatted paragraph(s)
  stack_border      sb;      // the border properties
  array<SI>         swell;   // swell properties for lines with large height

private:
  bool              unit_flag;      // did we just start a new unit ?
  int               unit_start;     // start of last unit
  stack_border      unit_sb;        // border properties of last unit
  array<page_item>  unit_ctrl;      // control items

  bool              no_break_flag;  // don't page break after next line
  int               no_break_begin; // start of non breaking zone

public:
  void set_env_vars (SI h, SI s, SI hs, SI vs, SI bot, SI top, array<SI> sw);
  void print (box b, array<lazy> fl= 0, int nr_cols= 1);
  void print (tree t, int nr_cols= 1, bool before= false);
  void print (space spc);
  void flush ();
  void penalty (int pen);
  void new_paragraph (space par_sep);
  void vspace_before (space spc);
  void vspace_after (space spc);
  void no_page_break_before ();
  void no_page_break_after ();
  void no_break_before ();
  void no_break_after ();
  void no_break_start ();
  void no_break_end ();

  stacker_rep ();
};

typedef stacker_rep* stacker;

array<page_item> typeset_stack (edit_env env, tree t, path ip,
				array<line_item> a, array<line_item> b,
				stack_border& sb);

void merge_stack (array<page_item>& l, stack_border& sb,
		  array<page_item> l2, stack_border sb2);

#endif // defined STACKER_H
