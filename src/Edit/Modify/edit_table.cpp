
/******************************************************************************
* MODULE     : edit_table.cpp
* DESCRIPTION: modify tables
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "edit_table.hpp"

/******************************************************************************
* Constructors and destructors
******************************************************************************/

edit_table_rep::edit_table_rep (): cell_mode ("cell") {}
edit_table_rep::~edit_table_rep () {}

/******************************************************************************
* Elementary subroutines for table manipulation
******************************************************************************/

static tree
empty_cell () {
  return "";
}

bool
is_empty_cell (tree t) {
  return
    is_empty (t) ||
    (is_compound (t, "cell-inert", 2) && is_empty_cell (t[1])) ||
    (is_compound (t, "cell-input", 3) && is_empty_cell (t[1])) ||
    (is_compound (t, "cell-output", 3) && is_empty_cell (t[2]));
}

static tree
empty_row (int nr_cols) {
  int i;
  tree R (ROW, nr_cols);
  for (i=0; i<nr_cols; i++)
    R[i]= tree (CELL, empty_cell ());
  return R;
}

static tree
empty_table (int nr_rows, int nr_cols) {
  int i;
  tree T (TABLE, nr_rows);
  for (i=0; i<nr_rows; i++)
    T[i]= empty_row (nr_cols);
  return T;
}

static void
table_get_extents (tree T, int& nr_rows, int& nr_cols) {
  while (is_func (T, TFORMAT)) T= T[N(T)-1];
  nr_rows= N(T);
  T= T[0];
  while (is_func (T, TFORMAT)) T= T[N(T)-1];
  nr_cols= N(T);
}

static void
table_set (tree& T, int row, int col, tree t) {
  tree* ptr= &T;
  while (is_func (*ptr, TFORMAT)) ptr= & ((*ptr) [N(*ptr)-1]);
  ptr= & ((*ptr) [row]);
  while (is_func (*ptr, TFORMAT)) ptr= & ((*ptr) [N(*ptr)-1]);
  ptr= & ((*ptr) [col]);
  while (is_func (*ptr, TFORMAT)) ptr= & ((*ptr) [N(*ptr)-1]);
  if (is_func (*ptr, CELL, 1)) ptr= & ((*ptr) [0]);
  *ptr= t;
}

static tree
table_get (tree T, int row, int col) {
  while (is_func (T, TFORMAT)) T= T [N(T)-1];
  T= T [row];
  while (is_func (T, TFORMAT)) T= T [N(T)-1];
  T= T [col];
  while (is_func (T, TFORMAT)) T= T [N(T)-1];
  if (is_func (T, CELL, 1)) T= T [0];
  return T;
}

/******************************************************************************
* Searching the format, table, rows and cells
******************************************************************************/

path
edit_table_rep::search_format () {
  path p= search_table ();
  return search_format (p);
}

path
edit_table_rep::search_format (path p) {
  if (!(rp < p)) return path ();
  if (is_func (subtree (et, p), TFORMAT)) return p;
  if (is_func (subtree (et, path_up (p)), TFORMAT)) return path_up (p);
  return p;
}

path
edit_table_rep::search_format (int& row, int& col) {
  path p= search_table (row, col);
  if (is_nil (p)) return p;
  if (is_func (subtree (et, p), TFORMAT)) return p;
  if (is_func (subtree (et, path_up (p)), TFORMAT)) return path_up (p);
  insert_node (p * 0, TFORMAT);
  return p;
}

path
edit_table_rep::search_table () {
  return search_upwards (TABLE);
}

path
edit_table_rep::search_table (path fp) {
  tree st= subtree (et, fp);
  if (is_func (st, TABLE)) return fp;
  if (is_func (st, TFORMAT)) return search_table (fp * (N(st)-1));
  return path ();
}

path
edit_table_rep::search_table (int& row, int& col) {
  row= col= 0;
  path p= search_table ();
  if (is_nil (p)) return p;
  path q= p, r= tail (tp, N(p));

  if (N(r) <= 1) return path ();
  row= r->item;
  while (true) {
    if (is_nil (r)) return r;
    q= q * r->item;
    r= r->next;
    if (is_func (subtree (et, q), ROW)) break;
  }

  if (N(r) <= 1) return path ();
  col= r->item;
  while (true) {
    if (is_nil (r)) return r;
    q= q * r->item;
    r= r->next;
    if (!is_func (subtree (et, q), TFORMAT)) break;
  }

  return p;
}

path
edit_table_rep::search_row (path fp, int row) {
  fp= search_table (fp) * row;
  tree st= subtree (et, fp);
  if (!is_func (st, ROW))
    return search_row (fp, N(st)-1);
  return fp;
}

path
edit_table_rep::search_cell (path p, int col) {
  p= p * col;
  tree st= subtree (et, p);
  if (is_func (st, TFORMAT))
    return search_cell (p, N(st)-1);
  if (is_func (st, CELL, 1)) return p * 0;
  return p;
}

path
edit_table_rep::search_cell (path fp, int row, int col) {
  return search_cell (search_row (fp, row), col);
}

/******************************************************************************
* Analyzing with statements in format
******************************************************************************/

void
edit_table_rep::with_raw_read (tree with, int& i1, int& j1, int& i2, int& j2) {
  i1= as_int (with[0]);
  i2= as_int (with[1]);
  j1= as_int (with[2]);
  j2= as_int (with[3]);
}

void
edit_table_rep::with_decode (int nr_rows, int nr_cols,
                             int& i1, int& j1, int& i2, int& j2)
{
  i1= (i1>=0? i1-1: nr_rows+i1);
  i2= (i2> 0? i2-1: nr_rows+i2);
  j1= (j1>=0? j1-1: nr_cols+j1);
  j2= (j2> 0? j2-1: nr_cols+j2);
}

void
edit_table_rep::with_decode (int nr_rows, int nr_cols,
                             int& I1, int& J1, int& I2, int& J2,
                             int& i1, int& j1, int& i2, int& j2)
{
  i1= I1; j1= J1; i2= I2; j2=J2;
  with_decode (nr_rows, nr_cols, i1, j1, i2, j2);
}

void
edit_table_rep::with_read (tree with, int nr_rows, int nr_cols,
                           int& i1, int& j1, int& i2, int& j2)
{
  with_raw_read (with, i1, j1, i2, j2);
  with_decode (nr_rows, nr_cols, i1, j1, i2, j2);
}

void
edit_table_rep::with_read (tree with, int nr_rows, int nr_cols,
                           int& I1, int& J1, int& I2, int& J2,
                           int& i1, int& j1, int& i2, int& j2)
{
  with_raw_read (with, I1, J1, I2, J2);
  i1= I1; j1= J1; i2= I2; j2=J2;
  with_decode (nr_rows, nr_cols, i1, j1, i2, j2);
}

/******************************************************************************
* Formatting primitives
******************************************************************************/

tree
edit_table_rep::table_get_format (path fp) {
  tree fm= get_env_value (CELL_FORMAT, fp * 0);
  tree st= subtree (et, fp);
  return fm * st (0, N(st)-1);
}

void
edit_table_rep::table_set_format (path fp, string var, tree val) {
  table_del_format (fp, var);
  tree with (TWITH, var, val);
  tree st= subtree (et, fp);
  insert (fp * (N(st)-1), tree (TFORMAT, with));
}

tree
edit_table_rep::table_get_format (path fp, string var) {
  tree st= table_get_format (fp);
  int k, n= N(st);
  tree val= get_env_value (var);
  for (k=0; k<n; k++)
    if (is_func (st[k], TWITH, 2) && (st[k][0] == var))
      val= st[k][1];
  return val;
}

void
edit_table_rep::table_del_format (path fp, string var) {
  tree st= subtree (et, fp);
  int k, n= N(st);
  for (k=n-2; k>=0; k--)
    if (is_func (st[k], TWITH, 2))
      if ((var == "") || (var == st[k][0]))
        remove (fp * k, 1);
}

void
edit_table_rep::table_set_format (
  path fp, int I1, int J1, int I2, int J2, string var, tree val)
{
  table_del_format (fp, I1, J1, I2, J2, var);
  tree with (CWITH);
  with << as_string (I1) << as_string (I2)
       << as_string (J1) << as_string (J2)
       << var << val;
  tree st= subtree (et, fp);
  insert (fp * (N(st)-1), tree (TFORMAT, with));
}

tree
edit_table_rep::table_get_format (
  path fp, int I1, int J1, int I2, int J2, string var)
{
  int i1, j1, i2, j2;
  int nr_rows, nr_cols;
  tree st= table_get_format (fp);
  table_get_extents (fp, nr_rows, nr_cols);
  with_decode (nr_rows, nr_cols, I1, J1, I2, J2, i1, j1, i2, j2);

  int k, n= N(st);
  tree val= get_env_value (var);
  for (k=0; k<n; k++)
    if (is_func (st[k], CWITH, 6) && (st[k][4] == var)) {
      int row1, col1, row2, col2;
      with_read (st[k], nr_rows, nr_cols, row1, col1, row2, col2);
      if ((row1<=i1) && (col1<=j1) && (row2>=i2) && (col2>=j2))
        val= st[k][5];
    }
  return val;
}

void
edit_table_rep::table_del_format (
  path fp, int I1, int J1, int I2, int J2, string var)
{
  int i1, j1, i2, j2;
  int nr_rows, nr_cols;
  tree st= subtree (et, fp);
  table_get_extents (fp, nr_rows, nr_cols);
  with_decode (nr_rows, nr_cols, I1, J1, I2, J2, i1, j1, i2, j2);

  int k, n= N(st);
  for (k=n-2; k>=0; k--)
    if (is_func (st[k], CWITH, 6))
      if ((var == "") || (var == st[k][4])) {
        int row1, col1, row2, col2;
        with_read (st[k], nr_rows, nr_cols, row1, col1, row2, col2);
        if ((row1>=i1) && (col1>=j1) && (row2<=i2) && (col2<=j2))
          remove (fp * k, 1);
      }
}

void
edit_table_rep::table_get_format (
  path fp, string var, tree** val, int nr_rows, int nr_cols)
{
  int i, j;
  tree def_val= get_env_value (var, fp);
  for (i=0; i<nr_rows; i++)
    for (j=0; j<nr_cols; j++)
      val[i][j]= def_val;

  tree st= table_get_format (fp);
  int k, n= N(st);
  for (k=0; k<n; k++)
    if (is_func (st[k], CWITH, 6) && (st[k][4] == var)) {
      int row1, col1, row2, col2;
      with_read (st[k], nr_rows, nr_cols, row1, col1, row2, col2);
      for (i=row1; i<=row2; i++)
        for (j=col1; j<=col2; j++)
          val[i][j]= st[k][5];
    }
}

void
edit_table_rep::table_individualize (path fp, string var) {
  int nr_rows, nr_cols;
  tree st= subtree (et, fp);
  table_get_extents (fp, nr_rows, nr_cols);

  int k, n= N(st);
  for (k=n-2; k>=0; k--)
    if (is_func (st[k], CWITH, 6))
      if ((var == "") || (var == st[k][4])) {
        int i, j, row1, col1, row2, col2;
        with_read (st[k], nr_rows, nr_cols, row1, col1, row2, col2);
        if ((row1==row2) && (col1==col2)) continue;
        row1= max (row1, 0); row2= min (row2, nr_rows-1);
        col1= max (col1, 0); col2= min (col2, nr_cols-1);
        tree ins_format (TFORMAT);
        for (i=row1; i<=row2; i++)
          for (j=col1; j<=col2; j++) {
            tree with (CWITH);
            with << as_string (i+1) << as_string (i+1)
                 << as_string (j+1) << as_string (j+1)
                 << copy (st[k][4]) << copy (st[k][5]) << copy (st[k][6]);
            ins_format << with;
          }
        remove (fp * k, 1);
        insert (fp * k, ins_format);
      }
}

void
edit_table_rep::table_format_center (path fp, int row, int col) {
  int nr_rows, nr_cols;
  tree st= subtree (et, fp);
  table_get_extents (fp, nr_rows, nr_cols);
  int Row1= row+1;
  int Col1= col+1;
  int Row2= row-nr_rows;
  int Col2= col-nr_cols;

  int k, n= N(st);
  for (k=n-2; k>=0; k--)
    if (is_func (st[k], CWITH, 6)) {
      int I1, I2, J1, J2, i1, i2, j1, j2;
      with_read (st[k], nr_rows, nr_cols, I1, J1, I2, J2, i1, j1, i2, j2);

      if (i1 == row) I1= Row1;
      else if (i1 < row) I1= i1+1;
      else I1= i1-nr_rows;
      if (i2 == row) I2= Row2;
      else if (i2 < row) I2= i2+1;
      else I2= i2-nr_rows;

      if (j1 == col) J1= Col1;
      else if (j1 < col) J1= j1+1;
      else J1= j1-nr_cols;
      if (j2 == col) J2= Col2;
      else if (j2 < col) J2= j2+1;
      else J2= j2-nr_cols;

      assign (fp * path (k, 0), as_string (I1));
      assign (fp * path (k, 1), as_string (I2));
      assign (fp * path (k, 2), as_string (J1));
      assign (fp * path (k, 3), as_string (J2));
    }
}

/******************************************************************************
* Inserting and deleteing new rows and columns
******************************************************************************/

void
edit_table_rep::table_get_extents (path fp, int& nr_rows, int& nr_cols) {
  ::table_get_extents (subtree (et, fp), nr_rows, nr_cols);
}

void
edit_table_rep::table_set_extents (path fp, int nr_rows, int nr_cols) {
  int old_rows, old_cols;
  table_get_extents (fp, old_rows, old_cols);
  if (nr_rows > old_rows || nr_cols > old_cols)
    table_insert (fp, old_rows, old_cols,
                  max (0, nr_rows - old_rows),
                  max (0, nr_cols - old_cols));
  if (nr_rows < old_rows || nr_cols < old_cols)
    table_remove (fp, nr_rows, nr_cols,
                  max (0, old_rows - nr_rows),
                  max (0, old_cols - nr_cols));
}

void
edit_table_rep::table_get_limits (
  path fp, int& i1, int& j1, int& i2, int& j2) 
{
  i1= max (1, as_int (table_get_format (fp, TABLE_MIN_ROWS)));
  j1= max (1, as_int (table_get_format (fp, TABLE_MIN_COLS)));
  i2= as_int (table_get_format (fp, TABLE_MAX_ROWS));
  j2= as_int (table_get_format (fp, TABLE_MAX_COLS));
  if (i2<i1) i2= 0x7fffffff;
  if (j2<i1) j2= 0x7fffffff;
}

void
edit_table_rep::table_remove (path fp, int row, int col, int delr, int delc) {
  path p= search_table (fp);
  int nr_rows, nr_cols;
  table_get_extents (p, nr_rows, nr_cols);
  tree T= subtree (et, p);
  if (delr>0)
    if (row+delr <= N(T)) {
      if (delr == N(T)) {
        destroy_table ();
        return;
      }
      remove (p * row, delr);
    }

  T= subtree (et, p);
  if (delc>0)
    for (row=0; row<N(T); row++) {
      path q= search_row (p, row);
      tree R= subtree (et, q);
      if (col+delc <= N(R)) {
        if (delc == N(R)) {
          destroy_table ();
          return;
        }
        remove (q * col, delc);
      }
    }

  tree st= subtree (et, fp);
  if (!is_func (st, TFORMAT)) return;
  int k, n= N(st);
  for (k=n-2; k>=0; k--)
    if (is_func (st[k], CWITH, 6)) {
      int I1, I2, J1, J2, i1, i2, j1, j2;
      with_read (st[k], nr_rows, nr_cols, I1, J1, I2, J2, i1, j1, i2, j2);
      if (delr>0) {
        if ((row<=i1) && (i2<row+delr)) { remove (fp * k, 1); continue; }
        if ((I1>0) && (i1>=row))
          assign (fp * path (k, 0), as_string (I1- min (delr, i1- row)));
        if ((I1<0) && (i1<row+delr))
          assign (fp * path (k, 0), as_string (I1+ delr+ min (0, row- 1- i1)));
        if ((I2>0) && (i2>=row))
          assign (fp * path (k, 1), as_string (I2- min (delr, i2- row)));
        if ((I2<0) && (i2<row+delr))
          assign (fp * path (k, 1), as_string (I2+ delr+ min (0, row- 1- i2)));
      }
      if (delc>0) {
        if ((col<=j1) && (j2<col+delc)) { remove (fp * k, 1); continue; }
        if ((J1>0) && (j1>=col))
          assign (fp * path (k, 2), as_string (J1- min (delc, j1- col)));
        if ((J1<0) && (j1<col+delc))
          assign (fp * path (k, 2), as_string (J1+ delc+ min (0, col- 1- j1)));
        if ((J2>0) && (j2>=col))
          assign (fp * path (k, 3), as_string (J2- min (delc, j2- col)));
        if ((J2<0) && (j2<col+delc))
          assign (fp * path (k, 3), as_string (J2+ delc+ min (0, col- 1- j2)));
      }
    }
}

void
edit_table_rep::table_insert (path fp, int row, int col, int insr, int insc) {
  path p= search_table (fp);
  int nr_rows, nr_cols;
  table_get_extents (p, nr_rows, nr_cols);
  tree T= subtree (et, p);
  if (insr>0)
    if (row <= N(T))
      insert (p * row, empty_table (insr, nr_cols));

  T= subtree (et, p);
  if (insc>0)
    for (row=0; row<N(T); row++) {
      path q= search_row (p, row);
      tree R= subtree (et, q);
      if (col <= N(R))
        insert (q * col, empty_row (insc));
    }

  tree st= subtree (et, fp);
  if (!is_func (st, TFORMAT)) return;
  int k, n= N(st);
  for (k=n-2; k>=0; k--)
    if (is_func (st[k], CWITH, 6)) {
      int I1, I2, J1, J2, i1, i2, j1, j2;
      with_read (st[k], nr_rows, nr_cols, I1, J1, I2, J2, i1, j1, i2, j2);
      if (insr>0) {
        bool flag= (I1<=0) || (I2>=0);
        if ((I1>0) && ((i1>row) || (flag && (i1==row))))
          assign (fp* path (k,0), as_string (I1+insr));
        if ((I1<0) && (i1<row))
          assign (fp* path (k,0), as_string (I1-insr));
        if ((I2>0) && (i2>=row))
          assign (fp* path (k,1), as_string (I2+insr));
        if ((I2<0) && ((i2<row-1) || (flag && (i2==row-1))))
          assign (fp* path (k,1), as_string (I2-insr));
      }
      if (insc>0) {
        bool flag= (J1<=0) || (J2>=0);
        if ((J1>0) && ((j1>col) || (flag && (j1==col))))
          assign (fp * path (k,2), as_string (J1+insc));
        if ((J1<0) && (j1<col))
          assign (fp * path (k,2), as_string (J1-insc));
        if ((J2>0) && (j2>=col))
          assign (fp * path (k,3), as_string (J2+insc));
        if ((J2<0) && ((j2<col-1) || (flag && (j2==col-1))))
          assign (fp * path (k,3), as_string (J2-insc));
      }
    }
}

/******************************************************************************
* Cursor positioning
******************************************************************************/

void
edit_table_rep::table_bound (
  path fp, int& row1, int& col1, int& row2, int& col2)
{
  fp= search_format (fp);
  if (!is_func (subtree (et, fp), TFORMAT)) return;

  int i, j, ii, jj, nr_rows, nr_cols;
  table_get_extents (fp, nr_rows, nr_cols);
  tree** rs= tm_new_array<tree*> (nr_rows);
  tree** cs= tm_new_array<tree*> (nr_rows);
  for (i=0; i<nr_rows; i++) {
    rs[i]= tm_new_array<tree> (nr_cols);
    cs[i]= tm_new_array<tree> (nr_cols);
  }
  table_get_format (fp, CELL_ROW_SPAN, rs, nr_rows, nr_cols);
  table_get_format (fp, CELL_COL_SPAN, cs, nr_rows, nr_cols);

  for (i=0; i<nr_rows; i++)
    for (j=0; j<nr_cols; j++) {
      int m= min (as_int (rs[i][j]), nr_rows-i);
      int n= min (as_int (cs[i][j]), nr_cols-j);
      if ((m>1) || (n>1)) {
        if ((row1 < i+m) && (col1 < j+n) && (row2 >= i) && (col2 >= j)) {
          row1= min (row1, i);
          col1= min (col1, j);
          row2= max (row2, i+m-1);
          col2= max (col2, j+n-1);
        }
        for (ii=0; ii<m; ii++)
          for (jj=0; jj<n; jj++) {
            rs[i+ii][j+jj]= "0";
            cs[i+ii][j+jj]= "0";
          }
      }
    }
  
  for (i=0; i<nr_rows; i++) {
    tm_delete_array (rs[i]);
    tm_delete_array (cs[i]);
  }
  tm_delete_array (rs);
  tm_delete_array (cs);
}

void
edit_table_rep::table_go_to (path fp, int row, int col, bool at_start) {
  int nr_rows, nr_cols;
  fp= search_format (fp);
  table_get_extents (fp, nr_rows, nr_cols);
  if (row<0) row= 0;
  if (col<0) col= 0;
  if (row>=nr_rows) row= nr_rows-1;
  if (col>=nr_cols) col= nr_cols-1;
  if (is_func (subtree (et, fp), TFORMAT)) {
    int row2= row, col2= col;
    table_bound (fp, row, col, row2, col2);
  }
  path q= search_cell (fp, row, col);
  go_to_border (q, at_start);
}

void
edit_table_rep::table_go_to_border (path fp, bool right) {
  while ((rp < fp) && (is_func (subtree (et, path_up (fp)), TFORMAT)))
    fp= path_up (fp);
  if ((rp < fp) &&
      is_document (subtree (et, path_up (fp))) &&
      (rp < path_up (fp)) &&
      is_extension (subtree (et, path_up (fp, 2)), 1))
    fp= path_up (fp);
  if ((rp < fp) && is_extension (subtree (et, path_up (fp)), 1))
    fp= path_up (fp);
  if ((rp < fp) && is_func (subtree (et, path_up (fp)), SUBTABLE, 1))
    fp= path_up (fp);
  go_to_border (fp, right);
}

void
edit_table_rep::back_table (path p, bool forward) {
  while (true) {
    tree st= subtree (et, p);
    if (!is_func (st, TFORMAT)) break;
    if (!is_func (st [N(st)-1], TABLE)) {
      back_general (p, forward);
      return;
    }
    p= p * (N(st)-1);
  }
  while (rp < p) {
    tree st= subtree (et, p);
    if (is_func (st, TABLE)) break;
    p= path_up (p);
  }
  if (!(rp < p)) return;

  if (forward) table_go_to (p, 0, 0, true);
  else {
    int nr_rows, nr_cols;
    table_get_extents (p, nr_rows, nr_cols);
    table_go_to (p, nr_rows-1, nr_cols-1, false);
  }
}

void
edit_table_rep::back_in_table (tree t, path p, bool forward) {
  if (is_func (t, TFORMAT) &&
      (is_func (subtree (et, path_up (p, 2)), INACTIVE) || in_source ()))
    {
      remove_empty_argument (p, forward);
      return;
    }

  int i, j, row, col, nr_rows, nr_cols;
  p= search_table (row, col);
  if (is_nil (p)) return;
  table_get_extents (p, nr_rows, nr_cols);

  bool flag=true;
  for (j=0; j<nr_cols; j++) {
    path q= search_cell (p, row, j);
    flag= flag && is_empty_cell (subtree (et, q));
  }
  if (flag) {
    int i1, j1, i2, j2;
    path fp= search_format ();
    if (is_nil (fp)) return;
    table_get_limits (fp, i1, j1, i2, j2);
    if (nr_rows-1 >= i1) {
      table_remove_row (forward, true);
      return;
    }
  }

  flag= true;
  for (i=0; i<nr_rows; i++) {
    path q= search_cell (p, i, col);
    flag= flag && is_empty_cell (subtree (et, q));
  }
  if (flag) {
    int i1, j1, i2, j2;
    path fp= search_format ();
    if (is_nil (fp)) return;
    table_get_limits (fp, i1, j1, i2, j2);
    if (nr_cols-1 >= j1) {
      table_remove_column (forward, true);
      return;
    }
  }

  flag=true;
  for (i=0; i<nr_rows; i++)
    for (j=0; j<nr_cols; j++) {
      path q= search_cell (p, i, j);
      flag= flag && is_empty_cell (subtree (et, q));
    }
  if (flag) {
    destroy_table ();
    return;
  }

  if (forward) {
    if (col<(nr_cols-1)) { table_go_to (p, row, col+1, true); return; }
    if (row<(nr_rows-1)) { table_go_to (p, row+1, 0, true); return; }
  }
  else {
    if (col>0) { table_go_to (p, row, col-1, false); return; }
    if (row>0) { table_go_to (p, row-1, nr_cols-1, false); return; }
  }
  table_go_to_border (p, !forward);
}

/******************************************************************************
* Routines for subtables
******************************************************************************/

tree
edit_table_rep::table_get_subtable (
  path fp, int row1, int col1, int row2, int col2)
{
  return table_get_subtable (fp, row1, col1, row2, col2, false);
}

tree
edit_table_rep::table_get_subtable (
  path fp, int row1, int col1, int row2, int col2, bool recurse)
{
  path p= search_table (fp);
  int i, j, nr_rows, nr_cols;
  table_get_extents (p, nr_rows, nr_cols);
  tree st= subtree (et, p);
  tree subtable (TABLE, row2+1-row1);
  for (i=row1; i<=row2; i++) {
    tree sr= st[i];
    tree sub_row (ROW, col2+1-col1);
    while (is_func (sr, TFORMAT)) sr= sr[N(sr)-1];
    for (j=col1; j<=col2; j++)
      sub_row[j-col1]= copy (sr[j]);
    subtable[i-row1]= sub_row;
  }

  st= subtree (et, fp);
  if ((!recurse) && (!is_func (st, TFORMAT))) return subtable;
  if (recurse) st= table_get_format (fp);
  else st= st (0, N(st)-1);
  int k, n= N(st);
  tree sub_format (TFORMAT);
  for (k=0; k<n; k++)
    if (is_func (st[k], CWITH, 6)) {
      int I1, I2, J1, J2, i1, i2, j1, j2;
      with_read (st[k], nr_rows, nr_cols, I1, J1, I2, J2, i1, j1, i2, j2);
      if ((i1<=row2) && (i2>=row1) && (j1<=col2) && (j2>=col1)) {
        I1= min (max (0, i1- row1), row2- row1) + 1;
        I2= min (max (0, i2- row1), row2- row1) + 1;
        J1= min (max (0, j1- col1), col2- col1) + 1;
        J2= min (max (0, j2- col1), col2- col1) + 1;
        tree with (CWITH);
        with << as_string (I1) << as_string (I2)
             << as_string (J1) << as_string (J2)
             << copy (st[k][4]) << copy (st[k][5]);
        sub_format << with;
      }
    }
  sub_format << subtable;
  return sub_format;
}

static tree
shift_subtable (tree st, int sh_row, int sh_col) {
  st= copy (st);
  int k, n= N(st);
  for (k=0; k<n-1; k++)
    if (is_func (st[k], CWITH, 6)) {
      st[k][0]= as_string (as_int (st[k][0]) + sh_row);
      st[k][1]= as_string (as_int (st[k][1]) + sh_row);
      st[k][2]= as_string (as_int (st[k][2]) + sh_col);
      st[k][3]= as_string (as_int (st[k][3]) + sh_col);
    }
  return st;
}

void
edit_table_rep::table_write_subtable (
  path fp, int row, int col, tree subt)
{
  int nr_rows, nr_cols, sub_rows, sub_cols;
  int min_rows, min_cols, max_rows, max_cols;
  table_get_extents (fp, nr_rows, nr_cols);
  ::table_get_extents (subt, sub_rows, sub_cols);
  table_get_limits (fp, min_rows, min_cols, max_rows, max_cols);
  if ((max_rows < row + sub_rows) || (max_cols < col + sub_cols)) return;
  if ((nr_rows < row + sub_rows) || (nr_cols < col + sub_cols))
    table_set_extents (fp, max (nr_rows, row + sub_rows),
                           max (nr_cols, col + sub_cols));

  path old_tp= tp;
  tp= fp * 0;
  bool calc_flag= inside ("calc-table");
  tp= old_tp;
  if (calc_flag)
    subt= as_tree (call ("calc-table-renumber", object (subt),
                         object (row + 1), object (col + 1)));

  if (is_func (subtree (et, fp), TFORMAT) &&
      is_func (subt, TFORMAT))
    {
      tree sh_subt= shift_subtable (subt, row, col);
      sh_subt= sh_subt (0, N(sh_subt)-1);
      tree st= subtree (et, fp);
      insert (fp * (N(st)-1), sh_subt);
      subt= subt [N(subt)-1];
    }

  int i, j;
  for (i=0; i<sub_rows; i++) {
    path rp  = search_row (fp, i+row);
    tree subr= subt[i];
    while (is_func (subr, TFORMAT)) subr= subr [N(subr)-1];
    for (j=0; j<sub_cols; j++) {
      path cp  = search_cell (rp, j+col);
      tree subc= subr[j];
      while (is_func (subc, TFORMAT)) subc= subc [N(subr)-1];
      if (is_func (subc, CELL, 1)) subc= subc[0];
      assign (cp, copy (subc));
    }
  }
}

void
edit_table_rep::table_hor_insert_subtable (path fp, int col, tree subt) {
  int nr_rows, nr_cols, sub_rows, sub_cols;
  table_get_extents (fp, nr_rows, nr_cols);
  ::table_get_extents (subt, sub_rows, sub_cols);
  if (sub_rows != nr_rows) return;
  table_insert (fp, 0, col, 0, sub_cols);
  table_write_subtable (fp, 0, col, subt);
}

void
edit_table_rep::table_ver_insert_subtable (path fp, int row, tree subt) {
  int nr_rows, nr_cols, sub_rows, sub_cols;
  table_get_extents (fp, nr_rows, nr_cols);
  ::table_get_extents (subt, sub_rows, sub_cols);
  if (sub_cols != nr_cols) return;
  table_insert (fp, row, 0, sub_rows, 0);
  table_write_subtable (fp, row, 0, subt);
}

/******************************************************************************
* Decorations
******************************************************************************/

void
edit_table_rep::table_force_decoration (path fp, int row, int col) {
  row++; col++;
  tree old= table_get_format (fp, row, col, row, col, CELL_DECORATION);
  if (old == "") {
    tree f (TFORMAT, tree (TABLE, tree (ROW, tree (TMARKER))));
    table_set_format (fp, row, col, row, col, CELL_DECORATION, f);
  }
}

static void
search_decoration (tree T, int& row, int& col) {
  while (is_func (T, TFORMAT)) T= T [N(T)-1];
  for (row=0; row<N(T); row++) {
    tree R= T[row];
    while (is_func (R, TFORMAT)) R= R [N(R)-1];
    for (col=0; col<N(R); col++) {
      tree C= R[col];
      while (is_func (C, TFORMAT)) C= C [N(C)-1];
      if (C == tree (TMARKER)) return;
    }
  }
  FAILED ("decoration not found");
}

static tree
table_format_undecorate (tree st, int row, int col, int dec_row, int dec_col) {
  tree fm (TFORMAT);
  int k, n= N(st);
  for (k=0; k<n-1; k++)
    if ((as_int (st[k][0]) <= (row+1)) && (as_int (st[k][1]) >= (row+1)) &&
        (as_int (st[k][2]) <= (col+1)) && (as_int (st[k][3]) >= (col+1)))
      if (is_func (st[k], CWITH, 6) && (st[k][4] != CELL_DECORATION)) {
        tree with= copy (st[k]);
        with[0]= as_string (dec_row+1);
        with[1]= as_string (dec_row+1);
        with[2]= as_string (dec_col+1);
        with[3]= as_string (dec_col+1);
        fm << with;
      }
  return fm;
}

static tree
table_undecorate (tree st, int row, int col) {
  int k, n= N(st);
  for (k=0; k<n-1; k++)
    if ((as_int (st[k][0]) == (row+1)) && (as_int (st[k][2]) == (col+1)))
      if (is_func (st[k], CWITH, 6) && (st[k][4] == CELL_DECORATION)) {
        int dec_row= 0, dec_col= 0;
        tree T= copy (st[k][5]);
        search_decoration (T, dec_row, dec_col);
        table_set (T, dec_row, dec_col, table_get (st[n-1], row, col));
        tree F= table_format_undecorate (st, row, col, dec_row, dec_col);
        return F * T;
      }
  FAILED ("decoration not found");
  return "";
}

void
edit_table_rep::table_hor_decorate (path fp, int col, int cbef, int caft) {
  tree st= subtree (et, fp);
  if (!is_func (st, TFORMAT)) return;
  if (cbef+caft == 0) return;
  int i, j, k, nr_rows, nr_cols;
  path p= search_table (fp);
  table_get_extents (p, nr_rows, nr_cols);
  table_individualize (fp, CELL_DECORATION);

  for (i=0; i<nr_rows; i++)
    for (j=col-cbef; j<=col+caft; j++)
      table_force_decoration (fp, i, j);

  tree t1, t2;
  if (caft>0)
    t2= table_get_subtable (fp, 0, col+1   , nr_rows-1, col+caft, true);
  if (cbef>0)
    t1= table_get_subtable (fp, 0, col-cbef, nr_rows-1, col-1   , true);
  if (caft>0) table_remove (fp, 0, col+1   , 0, caft);
  if (cbef>0) table_remove (fp, 0, col-cbef, 0, cbef);
  col -= cbef;

  st= subtree (et, fp);
  int n= N(st);
  for (k=0; k<n-1; k++)
    if (is_func (st[k], CWITH, 6) && (st[k][4] == CELL_DECORATION)) {
      int i1, j1, i2, j2;
      with_read (st[k], nr_rows, nr_cols, i1, j1, i2, j2);
      if (j1 != col) continue;
      for (j=0; j<caft; j++) {
        tree inst= table_undecorate (t2, i1, j);
        int sub_rows, sub_cols;
        ::table_get_extents (st[k][5], sub_rows, sub_cols);
        table_hor_insert_subtable (fp * path (k, 5), sub_cols, inst);
      }
      for (j=cbef-1; j>=0; j--) {
        tree inst= table_undecorate (t1, i1, j);
        table_hor_insert_subtable (fp * path (k, 5), 0, inst);
      }
    }
}

void
edit_table_rep::table_ver_decorate (path fp, int row, int rbef, int raft) {
  tree st= subtree (et, fp);
  if (!is_func (st, TFORMAT)) return;
  if (rbef+raft == 0) return;
  int i, j, k, nr_rows, nr_cols;
  path p= search_table (fp);
  table_get_extents (p, nr_rows, nr_cols);
  table_individualize (fp, CELL_DECORATION);

  for (i=row-rbef; i<=row+raft; i++)
    for (j=0; j<nr_cols; j++)
      table_force_decoration (fp, i, j);

  tree t1, t2;
  if (raft>0)
    t2= table_get_subtable (fp, row+1   , 0, row+raft, nr_cols-1, true);
  if (rbef>0)
    t1= table_get_subtable (fp, row-rbef, 0, row-1   , nr_cols-1, true);
  if (raft>0) table_remove (fp, row+1   , 0, raft, 0);
  if (rbef>0) table_remove (fp, row-rbef, 0, rbef, 0);
  row -= rbef;

  st= subtree (et, fp);
  int n= N(st);
  for (k=0; k<n-1; k++)
    if (is_func (st[k], CWITH, 6) && (st[k][4] == CELL_DECORATION)) {
      int i1, j1, i2, j2;
      with_read (st[k], nr_rows, nr_cols, i1, j1, i2, j2);
      if (i1 != row) continue;
      for (i=0; i<raft; i++) {
        tree inst= table_undecorate (t2, i, j1);
        int sub_rows, sub_cols;
        ::table_get_extents (st[k][5], sub_rows, sub_cols);
        table_ver_insert_subtable (fp * path (k, 5), sub_rows, inst);
      }
      for (i=rbef-1; i>=0; i--) {
        tree inst= table_undecorate (t1, i, j1);
        table_ver_insert_subtable (fp * path (k, 5), 0, inst);
      }
    }
}

/******************************************************************************
* User interface
******************************************************************************/

void
edit_table_rep::make_table (int nr_rows, int nr_cols) {
  //cout << "make_table " << nr_rows << ", " << nr_cols << "\n";
  tree T= empty_table (nr_rows, nr_cols);
  path p (0, 0, 0, 0);
  tree format_T (TFORMAT, T);
  insert_tree (format_T, path (N(format_T)-1, p));

  int i1, j1, i2, j2;
  path fp= search_format ();
  if (is_nil (fp)) return;
  typeset_invalidate_env (); // FIXME: dirty hack for getting correct limits
  table_get_limits (fp, i1, j1, i2, j2);
  if ((nr_rows<i1) || (nr_cols<j1)) {
    T= empty_table (max (nr_rows, i1), max (nr_cols, j1));
    format_T= tree (TFORMAT, T);
    assign (fp, format_T);
    go_to (fp * path (N(format_T)-1, p));
  }

  string hyphen= as_string (table_get_format (fp, TABLE_HYPHEN));
  string block = as_string (table_get_format (fp, TABLE_BLOCK));
  if (hyphen == "y" || block == "yes") {
    path q= fp;
    if (is_extension (subtree (et, path_up (q)), 1)) q= path_up (q);
    tree st= subtree (et, path_up (q));
    if (is_document (st)) insert_node (fp * 0, DOCUMENT);
    else if (is_concat (st) && is_document (subtree (et, path_up (q, 2)))) {
      int n= N(st), l= last_item (q);
      insert_node (fp * 0, DOCUMENT);
      if (l != (n-1)) {
        split (path_inc (q));
        correct_concat (path_inc (path_up (q)));
      }
      if (l != 0) {
        split (q);
        correct_concat (path_inc (path_up (q)));
      }
      correct_concat (path_up (q));
    }
  }

  table_correct_block_content ();
  set_message (concat (kbd_shortcut ("(structured-insert-down)"),
                       ": new row",
                       kbd_shortcut ("(structured-insert-right)"),
                       ": new column"),
               "table");
}

void
edit_table_rep::make_subtable (int nr_rows, int nr_cols) {
  path cp= search_upwards (CELL);
  if (is_nil (cp)) return;
  tree T= empty_table (nr_rows, nr_cols);
  path p (0, 0, 0, 0);
  T= tree (TFORMAT, T);
  p= path (N(T)-1, p);
  T= tree (SUBTABLE, T);
  p= path (0, p);
  assign (cp * 0, T);
  go_to (cp * path (0, p));
  table_correct_block_content ();
  set_message (concat (kbd_shortcut ("(structured-insert-down)"),
                       ": new row",
                       kbd_shortcut ("(structured-insert-right)"),
                       ": new column"),
               "table");
}

void
edit_table_rep::destroy_table () {
  path fp= search_format ();
  if (is_nil (fp)) return;
  while (rp < fp) {
    tree st= subtree (et, path_up (fp));
    if (!is_func (st, TFORMAT)) break;
    fp= path_up (fp);
  }
  if ((rp < fp) &&
      is_document (subtree (et, path_up (fp))) &&
      (rp < path_up (fp)) &&
      is_extension (subtree (et, path_up (fp, 2)), 1))
    fp= path_up (fp);
  if ((rp < fp) && is_extension (subtree (et, path_up (fp)), 1))
    fp= path_up (fp);
  if ((rp < fp) && is_func (subtree (et, path_up (fp)), SUBTABLE, 1))
    fp= path_up (fp);
  assign (fp, "");
  correct (path_up (fp));
}

void
edit_table_rep::table_deactivate () {
  path fp= search_format ();
  if (is_nil (fp)) return;
  tree st= subtree (et, fp);
  if (!is_func (st, TFORMAT)) return;
  insert_node (fp * 0, INACTIVE);
  set_message ("return: reactivate", "deactivate table");
}

void
edit_table_rep::table_extract_format () {
  path fp= search_format ();
  if (is_nil (fp)) return;
  tree fm= table_get_format (fp);
  fm << "";
  if (is_extension (subtree (et, path_up (fp)), 1)) fp= path_up (fp);
  assign (fp, fm);
  go_to (fp * path (N(fm)-1, 0));
}

void
edit_table_rep::table_insert_row (bool forward) {
  int row, col;
  path fp= search_format (row, col);
  if (is_nil (fp)) return;
  int nr_rows, nr_cols, i1, j1, i2, j2;
  table_get_extents (fp, nr_rows, nr_cols);
  table_get_limits (fp, i1, j1, i2, j2);
  if (nr_rows+1 > i2) return;
  table_insert (fp, row + (forward? 1: 0), col, 1, 0);
  table_go_to (fp, row + (forward? 1: 0), col);
  table_correct_block_content ();
  table_resize_notify ();
}

void
edit_table_rep::table_insert_column (bool forward) {
  int row, col;
  path fp= search_format (row, col);
  if (is_nil (fp)) return;
  int nr_rows, nr_cols, i1, j1, i2, j2;
  table_get_extents (fp, nr_rows, nr_cols);
  table_get_limits (fp, i1, j1, i2, j2);
  if (nr_cols+1 > j2) return;
  table_insert (fp, row, col + (forward? 1: 0), 0, 1);
  table_go_to (fp, row, col + (forward? 1: 0));
  table_correct_block_content ();
  table_resize_notify ();
}

void
edit_table_rep::table_remove_row (bool forward, bool flag) {
  int row, col;
  path fp= search_format (row, col);
  if (is_nil (fp)) return;
  int nr_rows, nr_cols, i1, j1, i2, j2;
  table_get_extents (fp, nr_rows, nr_cols);
  table_get_limits (fp, i1, j1, i2, j2);
  if (nr_rows-1 < i1) destroy_table ();
  else if (flag) {
    table_remove (fp, row, col, 1, 0);
    int ncol= col;
    if ((!forward) && (col == 0)) ncol= nr_cols-1;
    if (forward && (col == nr_cols-1)) ncol= 0;
    table_go_to (fp, max (0, row + (forward? 0: -1)), ncol, forward);
  }
  else {
    if (!forward) row--;
    if (row >= 0) table_remove (fp, row, col, 1, 0);
    if (row < nr_rows-1 && forward) table_go_to (fp, row, col, forward);
    else if (forward || row < 0) table_go_to_border (fp, !forward);
  }
  table_correct_block_content ();
  table_resize_notify ();
}

void
edit_table_rep::table_remove_column (bool forward, bool flag) {
  int row, col;
  path fp= search_format (row, col);
  if (is_nil (fp)) return;
  int nr_rows, nr_cols, i1, j1, i2, j2;
  table_get_extents (fp, nr_rows, nr_cols);
  table_get_limits (fp, i1, j1, i2, j2);
  if (nr_cols-1 < j1) destroy_table ();
  else if (flag) {
    table_remove (fp, row, col, 0, 1);
    int ncol= max (0, col + (forward? 0: -1));
    if ((!forward) && (col == 0)) ncol= nr_cols-1;
    if (forward && (col == nr_cols-1)) ncol= 0;
    table_go_to (fp, row, ncol, forward);
  }
  else {
    if (!forward) col--;
    if (col >= 0) table_remove (fp, row, col, 0, 1);
    if (col < nr_cols-1 && forward) table_go_to (fp, row, col, forward);
    else if (forward || col < 0) table_go_to_border (fp, !forward);
  }
  table_correct_block_content ();
  table_resize_notify ();
}

int
edit_table_rep::table_nr_rows () {
  int nr_rows, nr_cols;
  path fp= search_format ();
  if (is_nil (fp)) return -1;
  table_get_extents (fp, nr_rows, nr_cols);
  return nr_rows;
}

int
edit_table_rep::table_nr_columns () {
  int nr_rows, nr_cols;
  path fp= search_format ();
  if (is_nil (fp)) return -1;
  table_get_extents (fp, nr_rows, nr_cols);
  return nr_cols;
}

array<int>
edit_table_rep::table_get_extents () {
  array<int> r;
  int nr_rows, nr_cols;
  path fp= search_format ();
  if (is_nil (fp)) return r;
  table_get_extents (fp, nr_rows, nr_cols);
  r << nr_rows << nr_cols;
  return r;
}
  
void
edit_table_rep::table_set_extents (int rows, int cols) {
  path fp= search_format ();
  if (is_nil (fp)) return;
  int min_rows, min_cols, max_rows, max_cols;
  table_get_limits (fp, min_rows, min_cols, max_rows, max_cols);
  rows= min (max_rows, max (min_rows, rows));
  cols= min (max_cols, max (min_cols, cols));
  table_set_extents (fp, rows, cols);
}

int
edit_table_rep::table_which_row () {
  int row, col;
  path fp= search_format (row, col);
  if (is_nil (fp)) return 0;
  return row+1;
}

int
edit_table_rep::table_which_column () {
  int row, col;
  path fp= search_format (row, col);
  if (is_nil (fp)) return 0;
  return col+1;
}

array<int>
edit_table_rep::table_which_cells () {
  array<int> r;
  if (selection_active_table (false)) {
    int row1, col1, row2, col2;
    path fp= selection_get_subtable (row1, col1, row2, col2);
    if (is_nil (fp)) return r;
    r << row1+1 << row2+1 << col1+1 << col2+1;
  }
  else {
    int row, col;
    path fp= search_format (row, col);
    if (is_nil (fp)) return array<int> ();
    row++; col++;
    r << row << row << col << col;
  }
  return r;
}

path
edit_table_rep::table_search_cell (int row, int col) {
  int nr_rows, nr_cols;
  path fp= search_format ();
  if (is_nil (fp)) return path ();
  table_get_extents (fp, nr_rows, nr_cols);
  if (row>0) row--; else row+=nr_rows;
  if (col>0) col--; else col+=nr_cols;
  if ((row<0) || (row>=nr_rows) || (col<0) || (col>=nr_cols)) return path ();
  return search_cell (fp, row, col);
}

void
edit_table_rep::table_go_to (int row, int col) {
  int nr_rows, nr_cols;
  path fp= search_format ();
  if (is_nil (fp)) return;
  table_get_extents (fp, nr_rows, nr_cols);
  if (row>0) row--; else row+=nr_rows;
  if (col>0) col--; else col+=nr_cols;
  if ((row<0) || (row>=nr_rows) || (col<0) || (col>=nr_cols)) return;
  table_go_to (fp, row, col);
}

void
edit_table_rep::table_set_format (string var, tree val) {
  if (val == "") table_del_format (var);
  else if (selection_active_table (false)) {
    int row1, col1, row2, col2;
    path fp= selection_get_subtable (row1, col1, row2, col2);
    if (is_nil (fp)) return;
    table_set_format (fp, var, val);
  }
  else {
    path fp= search_format ();
    if (is_nil (fp)) return;
    table_set_format (fp, var, val);
  }
}

tree
edit_table_rep::table_get_format () {
  path fp= search_format ();
  if (is_nil (fp)) return "";
  return table_get_format (fp);
}

string
edit_table_rep::table_get_format (string var) {
  path fp= search_format ();
  if (is_nil (fp)) return "";
  return as_string (table_get_format (fp, var));
}

void
edit_table_rep::table_del_format (string var) {
  if (selection_active_table (false)) {
    int row1, col1, row2, col2;
    path fp= selection_get_subtable (row1, col1, row2, col2);
    if (is_nil (fp)) return;
    table_del_format (fp, var);
  }
  else {
    path fp= search_format ();
    if (is_nil (fp)) return;
    table_del_format (fp, var);
  }
}

void
edit_table_rep::table_format_center () {
  int row, col;
  path fp= search_format (row, col);
  if (is_nil (fp)) return;
  table_format_center (fp, row, col);
}

void
edit_table_rep::table_row_decoration (bool forward) {
  int row, col, nr_rows, nr_cols;
  path fp= search_format (row, col);
  if (is_nil (fp)) return;
  table_get_extents (fp, nr_rows, nr_cols);
  if ((!forward) && (row > 0)) table_ver_decorate (fp, row, 1, 0);
  if (forward && (row < (nr_rows-1))) table_ver_decorate (fp, row, 0, 1);
}

void
edit_table_rep::table_column_decoration (bool forward) {
  int row, col, nr_rows, nr_cols;
  path fp= search_format (row, col);
  if (is_nil (fp)) return;
  table_get_extents (fp, nr_rows, nr_cols);
  if ((!forward) && (col > 0)) table_hor_decorate (fp, col, 1, 0);
  if (forward && (col < (nr_cols-1))) table_hor_decorate (fp, col, 0, 1);
}

void
edit_table_rep::table_correct_block_content () {
  int nr_rows, nr_cols;
  path fp= search_format ();
  if (is_nil (fp)) return;
  table_get_extents (fp, nr_rows, nr_cols);
  int row, col;
  for (row= 0; row < nr_rows; row++)
    for (col= 0; col < nr_cols; col++) {
      path cp= search_cell (fp, row, col);
      tree st= subtree (et, cp);
      tree t1= table_get_format (fp, row+1, col+1, row+1, col+1, CELL_BLOCK);
      tree t2= table_get_format (fp, row+1, col+1, row+1, col+1, CELL_HYPHEN);
      bool f1= (t1 == "no" || (t1 == "auto" && t2 == "n"));
      bool f2= (t1 == "yes" || (t1 == "auto" && is_atomic (t2) && t2 != "n"));
      if (f1 && is_document (st) && N(st) == 1)
        remove_node (cp * 0);
      else if (f2 && !is_document (st))
        insert_node (cp * 0, DOCUMENT);
    }
}

void
edit_table_rep::table_resize_notify () {
  path p= search_table ();
  if (!is_nil (p))
    call ("table-resize-notify", object (subtree (et, p)));
}

void
edit_table_rep::set_cell_mode (string mode) {
  cell_mode= mode;
}

string
edit_table_rep::get_cell_mode () {
  return cell_mode;
}

void
edit_table_rep::cell_set_format (string var, tree val) {
  if (selection_active_table (false)) {
    int row1, col1, row2, col2, rows, cols;
    path fp= selection_get_subtable (row1, col1, row2, col2);
    row1++; col1++; row2++; col2++;
    table_get_extents (fp, rows, cols);
    if (rows > row1 && row1 <= 2 && row2 == rows) row2= -1;
    if (cols > col1 && col1 <= 2 && col2 == cols) col2= -1;
    table_set_format (fp, row1, col1, row2, col2, var, val);
  }
  else {
    int row, col;
    path fp= search_format (row, col); row++; col++;
    if (is_nil (fp)) return;
    if (cell_mode=="row")
      table_set_format (fp, row, 1, row, -1, var, val);
    else if (cell_mode=="column")
      table_set_format (fp, 1, col, -1, col, var, val);
    else if (cell_mode=="table")
      table_set_format (fp, 1, 1, -1, -1, var, val);
    else table_set_format (fp, row, col, row, col, var, val);
  }
  table_correct_block_content ();
}

string
edit_table_rep::cell_get_format (string var) {
  int row, col;
  path fp= search_format (row, col); row++; col++;
  if (is_nil (fp)) return "";
  if (cell_mode=="row")
    return as_string (table_get_format (fp, row, 1, row, -1, var));
  else if (cell_mode=="column")
    return as_string (table_get_format (fp, 1, col, -1, col, var));
  else if (cell_mode=="table")
    return as_string (table_get_format (fp, 1, 1, -1, -1, var));
  else return as_string (table_get_format (fp, row, col, row, col, var));
}

void
edit_table_rep::cell_del_format (string var) {
  if (selection_active_table (false)) {
    int row1, col1, row2, col2;
    path fp= selection_get_subtable (row1, col1, row2, col2);
    table_del_format (fp, row1+1, col1+1, row2+1, col2+1, var);
  }
  else {
    int row, col;
    path fp= search_format (row, col); row++; col++;
    if (is_nil (fp)) return;
    if (cell_mode=="row") table_del_format (fp, row, 1, row, -1, var);
    else if (cell_mode=="column") table_del_format (fp, 1, col, -1, col, var);
    else if (cell_mode=="table") table_del_format (fp, 1, 1, -1, -1, var);
    else table_del_format (fp, row, col, row, col, var);
  }
  table_correct_block_content ();
}

void
edit_table_rep::table_test () {
  path fp= search_format ();
  if (is_nil (fp)) return;
  cout << table_get_format (fp) << "\n";
}
