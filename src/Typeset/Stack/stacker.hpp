
/******************************************************************************
* MODULE     : stacker.hpp
* DESCRIPTION: Vertical formatting of paragraphs
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
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

private:
  bool              unit_flag;      // did we just start a new unit ?
  int               unit_start;     // start of last unit
  stack_border      unit_sb;        // border properties of last unit
  array<page_item>  unit_ctrl;      // control items

public:
  void set_env_vars (SI h, SI s, SI hs, SI vs, SI bot, SI top);
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

  stacker_rep ();
};

typedef stacker_rep* stacker;

array<page_item> typeset_stack (edit_env env, tree t, path ip,
				array<line_item> a, array<line_item> b,
				stack_border& sb);

void merge_stack (array<page_item>& l, stack_border& sb,
		  array<page_item> l2, stack_border sb2);

#endif // defined STACKER_H
