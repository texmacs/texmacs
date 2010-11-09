
/******************************************************************************
* MODULE     : edit_table.hpp
* DESCRIPTION: Editing matrices, tables and tables
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef EDIT_TABLE_H
#define EDIT_TABLE_H
#include "editor.hpp"

class edit_table_rep: virtual public editor_rep {
protected:
  string cell_mode;

protected:
  // Searching the format, a table, row or cell
  path search_format ();
  path search_format (path p);
  path search_format (int& row, int& col);
  path search_table ();
  path search_table (path fp);
  path search_table (int& row, int& col);
  path search_row (path fp, int row);
  path search_cell (path p, int col);
  path search_cell (path fp, int row, int col);

  // Analyzing with statements in the format
  void with_raw_read (tree with, int& i1, int& j1, int& i2, int& j2);
  void with_decode (int nr_rows, int nr_cols,
		    int& i1, int& j1, int& i2, int& j2);
  void with_decode (int nr_rows, int nr_cols,
		    int& I1, int& J1, int& I2, int& J2,
		    int& i1, int& j1, int& i2, int& j2);
  void with_read (tree with, int nr_rows, int nr_cols,
		  int& i1, int& j1, int& i2, int& j2);
  void with_read (tree with, int nr_rows, int nr_cols,
		  int& I1, int& J1, int& I2, int& J2,
		  int& i1, int& j1, int& i2, int& j2);
  
  // Routines for formatting tables
  tree table_get_format (path fp);
  void table_set_format (path fp, string var, tree val);
  tree table_get_format (path fp, string var);
  void table_del_format (path fp, string var);
  void table_set_format (path fp, int I1, int J1, int I2, int J2,
			 string var, tree val);
  tree table_get_format (path fp, int I1, int J1, int I2, int J2, string var);
  void table_del_format (path fp, int I1, int J1, int I2, int J2, string var);
  void table_get_format (path fp, string var,
			 tree** val, int nr_rows, int nr_cols);
  void table_individualize (path fp, string var);
  void table_format_center (path fp, int row, int col);

  // Main routines for manipulating the entire table
  void table_get_extents (path fp, int& nr_rows, int& nr_cols);
  void table_get_limits (path fp, int& i1, int& j1, int& i2, int& j2);
  void table_insert (path fp, int row, int col, int nr_rows, int nr_cols);
  void table_remove (path fp, int row, int col, int nr_rows, int nr_cols);
  tree table_get_subtable (path p, int i1, int j1, int i2, int j2);
  tree table_get_subtable (path p, int i1, int j1, int i2, int j2, bool rec);
  void table_write_subtable (path fp, int row, int col, tree subt);
  void table_hor_insert_subtable (path fp, int col, tree subt);
  void table_ver_insert_subtable (path fp, int row, tree subt);

  // Cell decorations
  void table_force_decoration (path fp, int row, int col);
  void table_hor_decorate (path fp, int col, int cbef, int caft);
  void table_ver_decorate (path fp, int row, int rbef, int raft);

  // Positioning the cursor inside tables
  void table_bound (path fp, int& row1, int& col1, int& row2, int& col2);
  void table_go_to (path fp, int row, int col, bool at_start= false);
  void table_go_to_border (path fp, bool right);
  void back_table (path p, bool forward);
  void back_in_table (tree t, path p, bool forward);

public:
  edit_table_rep ();
  ~edit_table_rep ();

  void   make_table (int nr_rows, int nr_cols);
  void   make_subtable (int nr_rows, int nr_cols);
  void   destroy_table ();
  void   table_disactivate ();
  void   table_extract_format ();
  void   table_insert_row (bool forward);
  void   table_insert_column (bool forward);
  void   table_remove_row (bool forward, bool flag= false);
  void   table_remove_column (bool forward, bool flag= false);
  int    table_nr_rows ();
  int    table_nr_columns ();
  int    table_which_row ();
  int    table_which_column ();
  path   table_search_cell (int row, int col);
  void   table_go_to (int row, int col);
  void   table_set_format (string var, tree val);
  string table_get_format (string var);
  void   table_del_format (string var);
  void   table_format_center ();
  void   table_row_decoration (bool forward);
  void   table_column_decoration (bool forward);
  void   table_correct_block_content ();
  void   set_cell_mode (string mode);
  string get_cell_mode ();
  void   cell_set_format (string var, tree val);
  string cell_get_format (string var);
  void   cell_del_format (string var);

  void   table_test ();
};

#endif // defined EDIT_TABLE_H
