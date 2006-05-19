
/******************************************************************************
* MODULE     : table.hpp
* DESCRIPTION: Tables and cells of tables
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef TABLE_H
#define TABLE_H
#include "typesetter.hpp"
#include "formatter.hpp"
#include "Format/line_item.hpp"
#include "Format/page_item.hpp"
#include "Format/stack_border.hpp"

class cell;
class table;

class table_rep: public concrete_struct {
protected:
  hashmap<string,tree> var;   // formatting variables

public:
  edit_env env;               // the environment
  int      status;            // 0: table, 1: decoration, 2: subtable
  int      i0;                // origin row
  int      j0;                // origin column
  path     ip;                // the corresponding source location
  box      b;                 // the resulting box
  SI       x1;                // left limit of box    (for subtables)
  SI       y1;                // bottom limit of box  (for subtables)
  SI       x2;                // right limit of box   (for subtables)
  SI       y2;                // top limit of box     (for subtables)

  cell**   T;                 // the cells of the table
  int      nr_rows;           // the number of rows
  int      nr_cols;           // the number of columns

  SI       width;             // the (suggested) width of the table
  SI       height;            // the (suggested) height of the table
  SI       lsep;              // left padding around table
  SI       rsep;              // right padding around table
  SI       bsep;              // bottom padding around table
  SI       tsep;              // top padding around table
  SI       lborder;           // left border width
  SI       rborder;           // right border width
  SI       bborder;           // bottom border width
  SI       tborder;           // top border width
  string   hmode;             // how to interpret the width 
  string   vmode;             // how to interpret the height
  string   halign;            // horizontal alignment
  string   valign;            // vertical alignment
  string   hyphen;            // vertical hypenation
  int      row_origin;        // row span (not yet implemented)
  int      col_origin;        // column span (not yet implemented)

  table_rep (edit_env env, int status, int i0, int j0);
  ~table_rep ();
  void display (bool flag= true);

  void typeset (tree t, path ip);
  void typeset_table (tree fm, tree t, path ip);
  void typeset_row (int i, tree fm, tree t, path ip);
  void format_table (tree fm);
  void format_item (tree with);
  void handle_decorations ();
  void handle_span ();
  void merge_borders ();
  void compute_width (SI& mw, SI& lw, SI& rw);
  void compute_widths (SI* mw, SI* lw, SI* rw, bool large);
  void compute_horizontal_parts (double* parts);
  void position_columns ();
  void compute_height (SI& mh, SI& bh, SI& th);
  void compute_heights (SI* mh, SI* bh, SI* th);
  void compute_vertical_parts (double* parts);
  void position_rows ();
  void finish_horizontal ();
  void finish ();
  array<box> var_finish ();

  friend class lazy_table_rep;
};

class table {
  CONCRETE_NULL(table);
  inline table (edit_env env, int status= 0, int i0= 0, int j0= 0):
    rep (new table_rep (env, status, i0, j0)) {}
};
CONCRETE_NULL_CODE(table);

class cell_rep: public concrete_struct {
protected:
  hashmap<string,tree> var;   // formatting variables

public:
  edit_env env;               // the environment
  path     ip;                // source location of cell
  lazy     lz;                // lazily typesetted cell
  box      b;                 // the resulting box
  SI       xoff;              // xoffset after positioning of the columns
  SI       yoff;              // yoffset after positioning of the rows
  SI       x1;                // lower left coordinate of cell
  SI       y1;                // upper left coordinate of cell
  SI       x2;                // lower right coordinate of cell
  SI       y2;                // upper right coordinate of cell
  SI       shift;             // shift when tables may be hyphenated

  tree     decoration;        // decoration
  string   bg;                // background color
  bool     orientation;       // portrait or landscape
  SI       width;             // the width of the cell
  SI       height;            // the height of the cell
  double   hpart;             // part in unused horizontal space
  double   vpart;             // part in unused vertical space
  SI       lsep;              // left padding
  SI       rsep;              // right padding
  SI       bsep;              // bottom padding
  SI       tsep;              // top padding
  SI       lborder;           // left border width
  SI       rborder;           // right border width
  SI       bborder;           // bottom border width
  SI       tborder;           // top border width
  string   hmode;             // how to interpret the width 
  string   vmode;             // how to interpret the height
  string   halign;            // horizontal alignment
  string   valign;            // vertical alignment
  string   vcorrect;          // vertical limits correction
  string   hyphen;            // horizontal hyphenation
  int      row_span;          // row span
  int      col_span;          // column span
  table    D;                 // potential decoration
  table    T;                 // potential subtable

  cell_rep (edit_env env);

  void typeset (tree fm, tree t, path ip);
  void format_cell (tree fm);
  void format_item (tree with);
  void compute_width (SI& mw, SI& lw, SI& rw, bool large);
  void compute_height (SI& mh, SI& bh, SI& th);
  void position_horizontally (SI offset, SI mw, SI lw, SI rw);
  void position_vertically (SI offset, SI mh, SI bh, SI th);
  void finish_horizontal ();
  void finish ();
};

class cell {
  CONCRETE_NULL(cell);
  inline cell (edit_env env): rep (new cell_rep (env)) {}
};
CONCRETE_NULL_CODE(cell);

#endif // defined TABLE_H
