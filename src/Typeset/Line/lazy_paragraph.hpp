
/******************************************************************************
* MODULE     : lazy_paragraph.hpp
* DESCRIPTION: Make lines of a paragraph from a typesetted concatenation
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef LAZY_PARAGRAPH_H
#define LAZY_PARAGRAPH_H
#include "formatter.hpp"
#include "Format/line_item.hpp"
#include "Format/format.hpp"
#include "Stack/stacker.hpp"
#include "tab.hpp"

#define MODE_JUSTIFY   0
#define MODE_LEFT      1
#define MODE_CENTER    2
#define MODE_RIGHT     3

struct lazy_paragraph_rep: public lazy_rep {
  edit_env             env;        // the environment
  array<line_item>     a;          // the line items to format
  hashmap<string,tree> style;      // the style parameters
  stacker_rep*         sss;        // the typesetted paragraph

protected:
  array<box>    items;       // the boxes on the line in order
  array<SI>     items_sp;    // the final spaces between boxes in lines
  array<space>  spcs;        // the spacing between the boxes of items
  array<lazy>   fl;          // floating insertions
  array<line_item> notes;    // line notes
  array<tab>    tabs;        // tabulations
  array<tree>   decs;        // decorations of the boxes on lines
  SI            cur_r;       // the current right offset of the last line unit
  space         cur_w;       // the current width of the line unit
  int           cur_start;   // index of the start of the line unit

  string        mode;        // justified, left, center or right
  double        flexibility; // threshold for switching to ragged mode
  string        hyphen;      // hyphenation quality
  double        kreduce;     // maximal kerning reduction
  double        kstretch;    // maximal kerning stretching
  int           protrusion;  // marginal kerning mode
  double        contraction; // maximal glyph contraction
  double        expansion;   // maximal glyph expansion
  SI            width;       // the width of the parbox
  SI            first;       // indentation of first line (added to left)
  SI            left;        // left indentation of paragraph lines
  SI            right;       // right indentation of paragraph lines
  SI            height;      // the minimal height of each line
  SI            sep;         // min. separation between ink of successive lines
  SI            hor_sep;     // min. hor. sep. when lines are shoved together
  SI            ver_sep;     // min. ver. sep. when lines are shoved together
  space         line_sep;    // space between consecutive lines
  space         par_sep;     // space between consecutive paragraphs
  SI            bot;         // next line will not ascend higher than this
  SI            top;         // previous line will not descend lower than this
  SI            tab_sep;     // separation between columns in tabular
  int           nr_cols;     // number of columns

  void line_print (line_item item);
  void line_print (line_item item, path start, path end);
  void line_print (path start, path end);

  void find_first_last_text (int& first, int& last);
  void protrude (bool lf, bool rf);
  array<box> adjusted (double factor, int first, int last);
  void adjust_kerning (SI dw, SI the_width);
  void expand_glyphs (SI dw, SI the_width);

  void handle_decoration (int& i, int& j, SI& xoff, box& b, SI& b_sp);
  void handle_decorations (int& i, int& j, SI& xoff,
			   array<box>& bs, array<SI>& bs_sp);
  void handle_decorations ();

  void line_start ();
  void make_unit (string mode, SI the_width, bool break_flag);
  void line_unit (path start, path end, bool break_flag,
		  string mode, SI the_left, SI the_right);
  void line_end (space spc, int penalty);
  void line_units (int start, int end, bool is_start, bool is_end,
		   string mode, string hyphen,
		   SI the_left, SI the_right, SI the_first, SI the_last);

  void format_paragraph_unit (int start, int end);

public:
  lazy_paragraph_rep (edit_env env, path ip);
  ~lazy_paragraph_rep ();
  operator tree ();
  void format_paragraph ();
  lazy produce (lazy_type request, format fm);
  format query (lazy_type request, format fm);
};

struct lazy_paragraph {
  EXTEND_NULL(lazy,lazy_paragraph);
  inline lazy_paragraph (edit_env env, path ip):
    rep (tm_new<lazy_paragraph_rep> (env, ip)) { rep->ref_count= 1; }
};
EXTEND_NULL_CODE(lazy,lazy_paragraph);

#endif // defined LAZY_PARAGRAPH_H
