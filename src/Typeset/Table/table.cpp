
/******************************************************************************
* MODULE     : table.cpp
* DESCRIPTION: Tables and cells of tables
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "Table/table.hpp"
#include "Boxes/construct.hpp"
#include "Format/format.hpp"
#include "analyze.hpp"

lazy make_lazy_paragraph (edit_env env, array<box> bs, path ip);

/******************************************************************************
* Tables
******************************************************************************/

table_rep::table_rep (edit_env env2, int status2, int i0b, int j0b):
  var (""), env (env2), status (status2), i0 (i0b), j0 (j0b),
  T (NULL), nr_rows (0), mw (NULL), lw (NULL), rw (NULL),
  width (0), height (0) {}

table_rep::~table_rep () {
  if (T != NULL) {
    int i;
    for (i=0; i<nr_rows; i++)
      if (T[i] != NULL) tm_delete_array (T[i]);
    tm_delete_array (T);
  }
  if (mw != NULL) tm_delete_array (mw);
  if (lw != NULL) tm_delete_array (lw);
  if (rw != NULL) tm_delete_array (rw);
}

void
table_rep::display (bool flag) {
  int i, j;
  if (flag) cout << HRULE << LF;
  else cout << "{ ";
  for (i=0; i<nr_rows; i++) {
    cout << "[ ";
    for (j=0; j<nr_cols; j++)
      if (!is_nil (T[i][j])) {
        cell C= T[i][j];
        if (j != 0) cout << ", ";
        if (!is_nil (C->b)) cout << C->b;
        else if (!is_nil (C->T)) {
          cout << "subtable ";
          C->T->display (false);
        }
        else cout << "nil";
        if (!is_nil (C->D)) {
          cout << " & decoration ";
          C->D->display (false);
        }
      }
    cout << " ]";
    if (flag) cout << LF;
    else if (i<nr_rows-1) cout << ", ";
  }
  if (flag) cout << HRULE << LF;
  else cout << " }";
}

void
table_rep::typeset (tree t, path iq) {
  ip= iq;
  tree old_format= env->local_begin (CELL_FORMAT, tree (TFORMAT));
  tree new_format= old_format;
  if (!is_func (new_format, TFORMAT)) new_format= tree (TFORMAT);
  while (is_func (t, TFORMAT)) {
    new_format= new_format * t (0, N(t)-1);
    iq        = descend (iq, N(t)-1);
    t         = t[N(t)-1];
  }
  format_table (new_format);
  typeset_table (new_format, t, iq);
  env->local_end (CELL_FORMAT, old_format);
}

void
table_rep::typeset_table (tree fm, tree t, path ip) {
  int i;
  nr_rows= N(t);
  nr_cols= 0;
  T= tm_new_array<cell*> (nr_rows);
  for (i=0; i<nr_rows; i++) T[i]= NULL;
  STACK_NEW_ARRAY (subformat, tree, nr_rows);
  extract_format (fm, subformat, nr_rows);
  for (i=0; i<nr_rows; i++) {
    tree old= env->local_begin (CELL_ROW_NR, as_string (i));
    typeset_row (i, subformat[i], t[i], descend (ip, i));
    env->local_end (CELL_ROW_NR, old);
  }
  STACK_DELETE_ARRAY (subformat);
  mw= tm_new_array<SI> (nr_cols);
  lw= tm_new_array<SI> (nr_cols);
  rw= tm_new_array<SI> (nr_cols);
}

void
table_rep::typeset_row (int i, tree fm, tree t, path ip) {
  //ASSERT (i==0 || nr_cols == N(t), "inconsistent number of columns");
  int j;
  nr_cols= (i==0? N(t): min (nr_cols, N(t)));
  T[i]= tm_new_array<cell> (nr_cols);
  STACK_NEW_ARRAY (subformat, tree, nr_cols);
  extract_format (fm, subformat, nr_cols);
  for (j=0; j<nr_cols; j++) {
    cell& C= T[i][j];
    C= cell (env);
    if (i == 0) C->border_flags += 1;
    if (i == nr_rows-1) C->border_flags += 2;
    tree old= env->local_begin (CELL_COL_NR, as_string (j));
    C->typeset (subformat[j], t[j], descend (ip, j));
    env->local_end (CELL_COL_NR, old);
    C->row_span= min (C->row_span, nr_rows- i);
    C->col_span= min (C->col_span, nr_cols- j);
    if (hyphen == "y") C->row_span= 1;
  }
  STACK_DELETE_ARRAY (subformat);
}

/******************************************************************************
* Subtables
******************************************************************************/

bool is_subtable_macro (edit_env env, tree t, array<tree>& xfm);

bool
is_subtable_macro_body (edit_env env, tree t, array<tree>& xfm) {
  //cout << "Subtable macro body? " << t << LF;
  if (is_func (t, TFORMAT)) {
    xfm << A (t (0, N(t) - 1));
    return is_subtable_macro_body (env, t[N(t)-1], xfm);
  }
  else if (is_func (t, DOCUMENT, 1))
    return is_subtable_macro_body (env, t[0], xfm);
  else if (is_func (t, ARG)) return true;
  else {
    if (!is_subtable_macro (env, t, xfm)) return false;
    return is_subtable_macro_body (env, t[0], xfm);
  }
}

bool
is_subtable_macro (edit_env env, tree t, array<tree>& xfm) {
  //cout << "Subtable macro? " << t << LF;
  if (!is_compound (t) || N(t) != 1) return false;
  if (!env->provides (as_string (L(t)))) return false;
  tree def= env->read (as_string (L(t)));
  if (!is_func (def, MACRO, 2)) return false;
  return is_subtable_macro_body (env, def[1], xfm);
}

tree
inherited_format (hashmap<string,tree> cvar) {
  array<tree> inh;
  if (cvar->contains (CELL_LSEP))
    inh << tree (CWITH, "1", "-1", "1", "1",
                 CELL_LSEP, cvar[CELL_LSEP]);
  if (cvar->contains (CELL_RSEP))
    inh << tree (CWITH, "1", "-1", "-1", "-1",
                 CELL_RSEP, cvar[CELL_RSEP]);
  if (cvar->contains (CELL_BSEP))
    inh << tree (CWITH, "-1", "-1", "1", "-1",
                 CELL_BSEP, cvar[CELL_BSEP]);
  if (cvar->contains (CELL_TSEP))
    inh << tree (CWITH, "1", "1", "1", "-1",
                 CELL_TSEP, cvar[CELL_TSEP]);
  if (cvar->contains (CELL_VCORRECT))
    inh << tree (CWITH, "1", "-1", "1", "-1",
                 CELL_VCORRECT, cvar[CELL_VCORRECT]);
  if (cvar->contains (CELL_HYPHEN))
    inh << tree (CWITH, "1", "-1", "1", "-1",
                 CELL_HYPHEN, cvar[CELL_HYPHEN]);
  if (cvar->contains (CELL_BLOCK))
    inh << tree (CWITH, "1", "-1", "1", "-1",
                 CELL_BLOCK, cvar[CELL_BLOCK]);
  if (cvar->contains (CELL_SWELL))
    inh << tree (CWITH, "1", "-1", "1", "-1",
                 CELL_SWELL, cvar[CELL_SWELL]);
  return tree (TFORMAT, inh);
}


void
table_rep::typeset_subtable (tree t, path iq, hashmap<string,tree> cvar) {
  ip= iq;

  bool plain_flag= true;
  tree new_format= tree (TFORMAT);
  while (is_func (t, TFORMAT)) {
    new_format= new_format * t (0, N(t)-1);
    iq        = descend (iq, N(t)-1);
    t         = t[N(t)-1];
  }

  if (is_func (t, TABLE, 1) &&
      is_func (t[0], ROW, 1) &&
      is_func (t[0][0], CELL, 1)) {
    path siq= descend (descend (descend (iq, 0), 0), 0);
    tree st = t[0][0][0];
    if (is_func (st, DOCUMENT, 1)) {
      siq= descend (siq, 0);
      st = st[0];
    }
    array<tree> xfm;
    if (is_subtable_macro (env, st, xfm)) {
      new_format << xfm;
      iq= descend (siq, 0);
      t = st[0];
      plain_flag= false;
    }
  }

  if (plain_flag) {
    tree inh_format= inherited_format (cvar);
    new_format= inh_format * new_format;
  }
  
  while (is_func (t, TFORMAT)) {
    new_format= new_format * t (0, N(t)-1);
    iq        = descend (iq, N(t)-1);
    t         = t[N(t)-1];
  }

  tree old_format= env->local_begin (CELL_FORMAT, tree (TFORMAT));
  format_table (new_format);
  typeset_table (new_format, t, iq);
  env->local_end (CELL_FORMAT, old_format);
}

/******************************************************************************
* Table formatting variables
******************************************************************************/

void
table_rep::format_table (tree fm) {
  int i, l= N(fm);
  for (i=0; i<l; i++)
    format_item (fm[i]);

  if (var->contains (TABLE_WIDTH)) {
    width= env->as_length (env->exec (var[TABLE_WIDTH]));
    if (var->contains (TABLE_HMODE))
      hmode= as_string (env->exec (var[TABLE_HMODE]));
    else hmode= "exact";
  }
  else {
    width= 0;
    hmode= "auto";
  }
  if (var->contains (TABLE_HEIGHT)) {
    height= env->as_length (env->exec (var[TABLE_HEIGHT]));
    if (var->contains (TABLE_VMODE))
      vmode= as_string (env->exec (var[TABLE_VMODE]));
    else vmode= "exact";
  }
  else {
    height= 0;
    vmode = "auto";
  }
  if (var->contains (TABLE_LSEP))
    lsep= env->as_length (env->exec (var[TABLE_LSEP]));
  else lsep= 0;
  if (var->contains (TABLE_RSEP))
    rsep= env->as_length (env->exec (var[TABLE_RSEP]));
  else rsep= 0;
  if (var->contains (TABLE_BSEP))
    bsep= env->as_length (env->exec (var[TABLE_BSEP]));
  else bsep= 0;
  if (var->contains (TABLE_TSEP))
    tsep= env->as_length (env->exec (var[TABLE_TSEP]));
  else tsep= 0;
  if (var->contains (TABLE_LBORDER))
    lborder= env->as_length (env->exec (var[TABLE_LBORDER])) >> 1;
  else lborder= 0;
  if (var->contains (TABLE_RBORDER))
    rborder= env->as_length (env->exec (var[TABLE_RBORDER])) >> 1;
  else rborder= 0;
  if (var->contains (TABLE_BBORDER))
    bborder= env->as_length (env->exec (var[TABLE_BBORDER])) >> 1;
  else bborder= 0;
  if (var->contains (TABLE_TBORDER))
    tborder= env->as_length (env->exec (var[TABLE_TBORDER])) >> 1;
  else tborder= 0;
  if (var->contains (TABLE_HALIGN))
    halign= as_string (env->exec (var[TABLE_HALIGN]));
  else halign= "l";
  if (var->contains (TABLE_VALIGN))
    valign= as_string (env->exec (var[TABLE_VALIGN]));
  else valign= "f";
  if (var->contains (TABLE_HYPHEN))
    hyphen= as_string (env->exec (var[TABLE_HYPHEN]));
  else hyphen= "n";
  if (var->contains (TABLE_ROW_ORIGIN))
    row_origin= as_int (env->exec (var[TABLE_ROW_ORIGIN]));
  else row_origin= 1;
  if (var->contains (TABLE_COL_ORIGIN))
    col_origin= as_int (env->exec (var[TABLE_COL_ORIGIN]));
  else col_origin= 1;
}

void
table_rep::format_item (tree with) {
  if (is_func (with, TWITH, 2))
    var (as_string (with[0]))= with[1];
}

/******************************************************************************
* Handling decorations and span
******************************************************************************/

void
table_rep::handle_decorations () {
  bool flag= true;
  int i, j, ii, jj, di, dj;
  array<int> ex_i1 (nr_rows), ex_i2 (nr_rows), off_i (nr_rows);
  array<int> ex_j1 (nr_cols), ex_j2 (nr_cols), off_j (nr_cols);

  /*** determine amount of decoration there is for each row and column ***/
  for (i=0; i<nr_rows; i++)
    ex_i1[i]= ex_i2[i]= 0;
  for (j=0; j<nr_cols; j++)
    ex_j1[j]= ex_j2[j]= 0;
  for (i=0; i<nr_rows; i++)
    for (j=0; j<nr_cols; j++) {
      cell C= T[i][j];
      if ((!is_nil (C)) && (!is_nil (C->T))) C->T->handle_decorations ();
      if ((!is_nil (C)) && (!is_nil (C->D))) {
        C->D->handle_decorations ();
        if (C->D->status == 1) {
          ii= i+ C->row_span- 1;
          jj= j+ C->col_span- 1;
          ex_i1[i ]= max (ex_i1[i ], C->D->i0);
          ex_j1[j ]= max (ex_j1[j ], C->D->j0);
          ex_i2[ii]= max (ex_i2[ii], C->D->nr_rows- 1- C->D->i0);
          ex_j2[jj]= max (ex_j2[jj], C->D->nr_cols- 1- C->D->j0);
          flag= false;
        }
      }
    }
  if (flag) return;
  for (i=0, ii=0; i<nr_rows; ii+=ex_i1[i]+ex_i2[i]+1, i++)
    off_i[i]= ii;
  for (j=0, jj=0; j<nr_cols; jj+=ex_j1[j]+ex_j2[j]+1, j++)
    off_j[j]= jj;

  /*** compute decorated table ***/
  cell** U;
  int new_rows= nr_rows, new_cols= nr_cols;
  for (i=0; i<nr_rows; i++) new_rows += ex_i1[i] + ex_i2[i];
  for (j=0; j<nr_cols; j++) new_cols += ex_j1[j] + ex_j2[j];
  U= tm_new_array<cell*> (new_rows);
  for (i=0; i<new_rows; i++)
    U[i]= tm_new_array<cell> (new_cols);

  for (i=0; i<nr_rows; i++)
    for (j=0; j<nr_cols; j++) {
      cell C= T[i][j];
      if (!is_nil (C)) {
        if ((!is_nil (C->D)) && (C->D->status==1)) {
          for (di=0; di<C->D->nr_rows; di++)
            for (dj=0; dj<C->D->nr_cols; dj++) {
              ii= di+ off_i[i]+ ex_i1[i]- C->D->i0;
              jj= dj+ off_j[j]+ ex_j1[j]- C->D->j0;
              U[ii][jj]= C->D->T[di][dj];
            }
          C->D= table ();
        }
        ii= off_i[i]+ ex_i1[i];
        jj= off_j[j]+ ex_j1[j];
        U[ii][jj]= C;
        ii= i+ C->row_span- 1;
        jj= j+ C->col_span- 1;
        C->row_span= off_i[ii]+ ex_i1[ii]+ 1- off_i[i]- ex_i1[i];
        C->col_span= off_j[jj]+ ex_j1[jj]+ 1- off_j[j]- ex_j1[j];
      }
    }

  /*** replace old table by new one ***/
  for (i=0; i<nr_rows; i++) tm_delete_array (T[i]);
  tm_delete_array (T);
  T      = U;
  nr_rows= new_rows;
  nr_cols= new_cols;
  i0     = off_i[i0] + ex_i1[i0];
  j0     = off_j[j0] + ex_j1[j0];
}

void
table_rep::handle_span () {
  int i, j, ii, jj;
  for (i=0; i<nr_rows; i++)
    for (j=0; j<nr_cols; j++) {
      cell C= T[i][j];
      if (!is_nil (C)) {
        for (ii=0; ii<C->row_span; ii++)
          for (jj=0; jj<C->col_span; jj++)
            if ((ii != 0) || (jj != 0))
              T[i+ii][j+jj]= cell ();
        if (!is_nil (C->T)) C->T->handle_span ();
      }
    }
}

/******************************************************************************
* Each border is the maximum of the borders of its adjacent cells
******************************************************************************/

void
table_rep::merge_borders () {
  int hh= nr_cols + 1, vv= nr_rows + 1, nn= hh * vv;
  array<SI> horb (nn), verb (nn);
  for (int i=0; i<nn; i++) horb[i]= verb[i]= 0;

  for (int i=0; i<nr_rows; i++)
    for (int j=0; j<nr_cols; j++) {
      cell C= T[i][j];
      if (!is_nil (C)) {
        for (int di=0; di < C->row_span; di++) {
          int ii= i+di, jj= j, kk= ii*hh + jj;
          horb[kk]= max (horb[kk], C->lborder);
          jj= j + C->col_span; kk= ii*hh + jj;
          horb[kk]= max (horb[kk], C->rborder);
        }
        for (int dj=0; dj < C->col_span; dj++) {
          int ii= i, jj= j+dj, kk= ii*hh + jj;
          verb[kk]= max (verb[kk], C->tborder);
          ii= i + C->row_span; kk= ii*hh + jj;
          verb[kk]= max (verb[kk], C->bborder);
        }
      }
    }

  for (int i=0; i<nr_rows; i++)
    for (int j=0; j<nr_cols; j++) {
      cell C= T[i][j];
      if (!is_nil (C)) {
        SI lb= 0, rb= 0, bb= 0, tb= 0;
        for (int di=0; di < C->row_span; di++) {
          int ii= i+di, jj= j, kk= ii*hh + jj;
          lb= max (horb[kk], lb);
          jj= j + C->col_span; kk= ii*hh + jj;
          rb= max (horb[kk], rb);
        }
        for (int dj=0; dj < C->col_span; dj++) {
          int ii= i, jj= j+dj, kk= ii*hh + jj;
          tb= max (verb[kk], tb);
          ii= i + C->row_span; kk= ii*hh + jj;
          bb= max (verb[kk], bb);
        }
        C->lborder= lb; C->rborder= rb;
        C->bborder= bb; C->tborder= tb;
      }
    }
}

/******************************************************************************
* Subroutines for positioning
******************************************************************************/

static SI
sum (SI* a, int n) {
  int i, s=0;
  for (i=0; i<n; i++) s+= a[i];
  return s;
}

static double
sum (double* a, int n) {
  int i;
  double s=0.0;
  for (i=0; i<n; i++) s+= a[i];
  return s;
}

static void
blow_up (SI* w, SI* l, SI* r, SI* W, SI* L, SI* R,
         SI room, double* part, int n, bool vert)
{
  int i;
  for (i=0; i<n; i++)
    if (w[i] >= W[i] || part[i] < 0)
      part[i]= 0;
  double total= sum (part, n);
  if (total <= 0) {
    for (i=0; i<n; i++) part[i]= 1;
    total= (double) n;
  }

  STACK_NEW_ARRAY (Part, double, n);
  for (i=0; i<n; i++) Part[i]= part[i];

  SI old_room= room;
  while (true) {
    for (i=0; i<n; i++)
      if (part[i] > 0) {
        SI extra= (SI) ((part[i]/total) * old_room);
        if (w[i]+extra >= W[i]) {
          room    -= (W[i]-w[i]);
          w[i]     = W[i];
          l[i]     = L[i];
          r[i]     = R[i];
          part[i]  = 0;
        }
      }
    total= sum (part, n);
    if (room <= 0) {
      STACK_DELETE_ARRAY (Part);
      return;
    }
    if ((total <= 0) || (room == old_room) || (room <= 0)) break;
    old_room= room;
  }

  if (total <= 0) {
    for (i=0; i<n; i++) part[i]= Part[i];
    total= sum (part, n);
  }
  for (i=0; i<n; i++) {
    SI extra = (SI) ((part[i]/total) * room);
    w[i] += extra;
    if (L[i] + R[i] >= w[i] && !vert) {
      SI s= l[i] + r[i];
      SI S= L[i] + R[i];
      double lambda= ((double) (w[i] - s)) / ((double) (S - s));
      l[i] += (SI) (lambda * (L[i] - l[i]));
      r[i]  = w[i] - l[i];
    }
    else {
      l[i]= L[i];
      r[i]= R[i];
    }
  }

  STACK_DELETE_ARRAY (Part);
}

/******************************************************************************
* Horizontal positioning
******************************************************************************/

void
table_rep::compute_widths (SI* Mw, SI* Lw, SI* Rw, bool large) {
  //cout << "Compute widths " << large << LF << INDENT;
  int i, j;
  for (j=0; j<nr_cols; j++)
    Mw[j]= Lw[j]= Rw[j]= 0;

  for (j=0; j<nr_cols; j++)
    for (i=0; i<nr_rows; i++) {
      cell C= T[i][j];
      if ((!is_nil (C)) && (C->col_span == 1)) {
        SI cmw, clw, crw;
        C->compute_width (cmw, clw, crw, large);
        //cout << i << ", " << j << ": "
        //     << (cmw>>8) << "; " << (clw>>8) << ", " << (crw>>8) << LF;
        Mw[j]= max (Mw[j], cmw);
        Lw[j]= max (Lw[j], clw);
        Rw[j]= max (Rw[j], crw);
        Mw[j]= max (Mw[j], Lw[j] + Rw[j]);
      }
    }

  for (j=0; j<nr_cols; j++)
    for (i=0; i<nr_rows; i++) {
      cell C= T[i][j];
      if ((!is_nil (C)) && (C->col_span != 1)) {
        SI cmw, clw, crw;
        C->compute_width (cmw, clw, crw, large);
        SI tot= sum (Mw+j, C->col_span);
        if (cmw > tot) Mw[j] += cmw - tot;
      }
    }
  //cout << UNINDENT << "Computed widths" << LF;
}

void
table_rep::compute_horizontal_parts (double* part) {
  int i, j;
  for (j=0; j<nr_cols; j++) part[j]= 0.0;
  for (j=0; j<nr_cols; j++)
    for (i=0; i<nr_rows; i++) {
      cell C= T[i][j];
      if (!is_nil (C))
        part[j]= max (part[j], C->hpart);
    }
}

void
table_rep::position_columns (bool large) {
  //cout << HRULE << LF;
  //cout << "Position columns " << (width >> 8) << ", " << large << LF << INDENT;
  compute_widths (mw, lw, rw, hmode == "auto" && large);
  //for (int i=0; i<nr_cols; i++)
  //  cout << "Column " << i << ": " << (mw[i]>>8) << "; "
  //       << (lw[i]>>8) << ", " << (rw[i]>>8) << LF;
  if (hmode != "auto" && large) {
    SI min_width= sum (mw, nr_cols);
    SI hextra= width - min_width;
    //cout << "Extra horizontal " << (hextra >> 8) << LF;
    if (hextra > 0) {
      STACK_NEW_ARRAY (part, double, nr_cols);
      STACK_NEW_ARRAY (Mw, SI, nr_cols);
      STACK_NEW_ARRAY (Lw, SI, nr_cols);
      STACK_NEW_ARRAY (Rw, SI, nr_cols);
      compute_horizontal_parts (part);
      //for (int i=0; i<nr_cols; i++)
      //  cout << "Part " << i << "= " << part[i] << LF;
      compute_widths (Mw, Lw, Rw, true);
      SI max_width= sum (Mw, nr_cols);
      SI computed_width= width;
      if (hmode == "min") computed_width= min (width, max_width);
      if (hmode == "max") computed_width= max (width, min_width);
      hextra= max (computed_width - min_width, 0);
      //for (int i=0; i<nr_cols; i++)
      //  cout << "Column " << i << ": "
      //       << (mw[i]>>8) << "; "
      //       << (lw[i]>>8) << ", " << (rw[i]>>8) << LF
      //       << "          "
      //       << (Mw[i]>>8) << "; "
      //       << (Lw[i]>>8) << ", " << (Rw[i]>>8) << LF;
      blow_up (mw, lw, rw, Mw, Lw, Rw, hextra, part, nr_cols, false);
      //for (int i=0; i<nr_cols; i++)
      //  cout << "Column " << i << ": " << (mw[i]>>8) << "; "
      //       << (lw[i]>>8) << ", " << (rw[i]>>8) << LF;
      STACK_DELETE_ARRAY (part);
      STACK_DELETE_ARRAY (Mw);
      STACK_DELETE_ARRAY (Lw);
      STACK_DELETE_ARRAY (Rw);
    }
  }

  int i, j;
  for (j=0; j<nr_cols; j++)
    for (i=0; i<nr_rows; i++) {
      cell C= T[i][j];
      if (!is_nil (C)) {
        if (!is_nil (C->T)) {
          C->T->width= mw[j]- C->lborder- C->rborder;
          C->T->hmode= "exact";
          C->T->position_columns (large);
        }
        else if (C->hyphen != "n")
          C->width= mw[j];
      }
    }

  SI xoff;
  if (halign == "l") xoff= 0;
  else if (halign == "c") xoff= -sum (mw, nr_cols) >> 1;
  else if (halign == "r") xoff= -sum (mw, nr_cols);
  else if (halign == "L") xoff= -lw[0];
  else if (halign == "C")
    xoff= -(sum (mw, nr_cols>>1) + sum (mw, (nr_cols-1)>>1)+
            lw[nr_cols>>1] + lw[(nr_cols-1)>>1]) >> 1;
  else if (halign == "R") xoff= -sum (mw, nr_cols - 1) - lw[nr_cols - 1];
  else if (halign == "O") {
    int orig= col_origin;
    if (orig < 0) orig += nr_cols;
    else orig--;
    orig= max (min (orig, nr_cols - 1), 0);
    xoff= -sum (mw, orig) - lw[orig];
  }
  else xoff= -sum (mw, j0) - lw[j0];

  x1= xoff- lborder- lsep;
  for (j=0; j<nr_cols; j++) xoff += mw[j];
  x2= xoff+ rborder+ rsep;
  //cout << UNINDENT << "Positioned columns" << LF;
}

void
table_rep::compute_width (SI& tmw, SI& tlw, SI& trw, bool large) {
  position_columns (large);
  tmw= x2 - x1;
  tlw= -x1;
  trw= x2;
}

/******************************************************************************
* Vertical positioning
******************************************************************************/

void
table_rep::compute_heights (SI* mh, SI* bh, SI* th, SI xh) {
  int i, j;
  for (i=0; i<nr_rows; i++)
    mh[i]= bh[i]= th[i]= 0;

  for (i=0; i<nr_rows; i++)
    for (j=0; j<nr_cols; j++) {
      cell C= T[i][j];
      if ((!is_nil (C)) && (C->row_span==1)) {
        SI cmh, cbh, cth;
        C->compute_height (cmh, cbh, cth, xh);
        mh[i]= max (mh[i], cmh);
        bh[i]= max (bh[i], cbh);
        th[i]= max (th[i], cth);
        mh[i]= max (mh[i], bh[i] + th[i]);
      }
    }

  for (i=0; i<nr_rows; i++)
    for (j=0; j<nr_cols; j++) {
      cell C= T[i][j];
      if ((!is_nil (C)) && (C->row_span!=1)) {
        SI cmh, cbh, cth;
        C->compute_height (cmh, cbh, cth, xh);
        SI tot= sum (mh+i, C->row_span);
        if (cmh > tot) mh[i] += cmh - tot;
      }
    }
}

void
table_rep::compute_vertical_parts (double* part) {
  int i, j;
  for (i=0; i<nr_rows; i++) part[i]= 0.0;
  for (i=0; i<nr_rows; i++)
    for (j=0; j<nr_cols; j++) {
      cell C= T[i][j];
      if (!is_nil (C))
        part[i]= max (part[i], C->vpart);
    }
}

void
table_rep::position_rows () {
  //cout << "Position rows" << LF << INDENT;
  STACK_NEW_ARRAY (mh, SI, nr_rows);
  STACK_NEW_ARRAY (bh, SI, nr_rows);
  STACK_NEW_ARRAY (th, SI, nr_rows);

  compute_heights (mh, bh, th, 0);
  if (vmode != "auto") {
    SI min_height= sum (mh, nr_rows);
    SI vextra= height - min_height;
    if (vextra > 0) {
      STACK_NEW_ARRAY (part, double, nr_rows);
      STACK_NEW_ARRAY (Mh, SI, nr_rows);
      STACK_NEW_ARRAY (Bh, SI, nr_rows);
      STACK_NEW_ARRAY (Th, SI, nr_rows);
      compute_vertical_parts (part);
      vextra= max (height - min_height, 0);
      //cout << "height = " << (height>>8) << LF;
      //cout << "vextra = " << (vextra>>8) << LF;
      //for (int i=0; i<nr_rows; i++)
      //  cout << "Part " << i << " = " << part[i] << LF;
      compute_heights (Mh, Bh, Th, vextra);
      SI max_height= sum (Mh, nr_rows);
      SI computed_height= height;
      if (vmode == "min") computed_height= min (height, max_height);
      if (vmode == "max") computed_height= max (height, min_height);
      vextra= max (computed_height - min_height, 0);
      //for (int i=0; i<nr_rows; i++)
      //  cout << "Row " << i << ": "
      //       << (mh[i]>>8) << "; "
      //       << (bh[i]>>8) << ", " << (th[i]>>8) << LF
      //       << "     : "
      //       << (Mh[i]>>8) << "; "
      //       << (Bh[i]>>8) << ", " << (Th[i]>>8) << LF;
      blow_up (mh, bh, th, Mh, Bh, Th, vextra, part, nr_rows, true);
      //for (int i=0; i<nr_rows; i++)
      //  cout << "Row " << i << ": " << (mh[i]>>8) << "; "
      //       << (bh[i]>>8) << ", " << (th[i]>>8) << LF;
      STACK_DELETE_ARRAY (part);
      STACK_DELETE_ARRAY (Mh);
      STACK_DELETE_ARRAY (Bh);
      STACK_DELETE_ARRAY (Th);
    }
  }

  int i, j;
  for (i=0; i<nr_rows; i++) {
    for (j=0; j<nr_cols; j++) {
      cell C= T[i][j];
      if ((!is_nil (C)) && (!is_nil (C->T))) {
        C->T->height= mh[i]- C->bborder- C->tborder;
        C->T->vmode = "exact";
        C->T->position_rows ();
      }
    }
  }

  SI yoff;
  if (valign == "t") yoff= 0;
  else if (valign == "c") yoff= sum (mh, nr_rows) >> 1;
  else if (valign == "f") yoff= (sum (mh, nr_rows) >> 1) + env->fn->yfrac;
  else if (valign == "b") yoff= sum (mh, nr_rows);
  else if (valign == "T") yoff= th[0];
  else if (valign == "C")
    yoff= (sum (mh, nr_rows>>1) + sum (mh, (nr_rows-1)>>1) +
           th[nr_rows>>1] + th[(nr_rows-1)>>1]) >> 1;
  else if (valign == "B") yoff= sum (mh, nr_rows - 1) + th[nr_rows - 1];
  else if (valign == "O") {
    int orig= row_origin;
    if (orig < 0) orig += nr_rows;
    else orig--;
    orig= max (min (orig, nr_rows - 1), 0);
    yoff= sum (mh, orig) + th[orig];
  }
  else yoff= sum (mh, i0) + th[i0];

  y2= yoff+ tborder+ tsep;
  for (i=0; i<nr_rows; i++) {
    for (j=0; j<nr_cols; j++) {
      cell C= T[i][j];
      if (!is_nil (C)) {
        SI tot= sum (mh+i, C->row_span);
        C->position_vertically (yoff, tot, tot+ bh[i]- mh[i], th[i]);
        C->shift= -yoff+ tot- bh[i];
      }
    }
    yoff -= mh[i];
  }
  y1= yoff- bborder+ bsep;

  STACK_DELETE_ARRAY (mh);
  STACK_DELETE_ARRAY (bh);
  STACK_DELETE_ARRAY (th);
  //cout << UNINDENT << "Positioned rows" << LF;
}

void
table_rep::compute_height (SI& tmh, SI& tbh, SI& tth, SI xh) {
  position_rows ();
  tmh= y2 - y1;
  tbh= -y1;
  tth= y2;
}

/******************************************************************************
* Generate table
******************************************************************************/

void
table_rep::finish_horizontal () {
  int i, j;
  SI xoff= x1 + lborder + lsep;
  for (j=0; j<nr_cols; j++) {
    for (i=0; i<nr_rows; i++) {
      cell C= T[i][j];
      if (!is_nil (C)) {
        if (C->col_span > 1)
          C->width += sum (mw+j+1, C->col_span-1);
        if (!is_nil (C->T))
          C->T->finish_horizontal ();
        else if (C->hyphen != "n")
          C->finish_horizontal ();
        SI tot= sum (mw+j, C->col_span);
        C->position_horizontally (xoff, tot, lw[j], rw[j+C->col_span-1]);
      }
    }
    xoff += mw[j];
  }
}

void
table_rep::finish () {
  int i, j;
  array<box>    bs;
  array<SI>     x;
  array<SI>     y;
  array<string> ha;
  bool ext_flag= true;
  for (i=0; i<nr_rows; i++)
    for (j=0; j<nr_cols; j++)
      if (!is_nil (T[i][j])) {
        cell C = T[i][j];
        C->finish ();
        bs << C->b;
        x  << C->x1;
        y  << C->y1;
        ha << C->halign;
        if (N(ha) == 0) ext_flag= false;
        else if (ha[0] != 'l' && ha[0] != 'c' && ha[0] != 'r') ext_flag= false;
      }
      else ext_flag= false;

  box tb;
  if (ext_flag) tb= table_box (ip, bs, x, y, ha, nr_cols);
  else tb= composite_box (ip, bs, x, y, false);

  SI    x1= tb->x1;
  SI    y1= tb->y1;
  SI    x2= tb->x2;
  SI    y2= tb->y2;
  brush fg= env->pen->get_brush ();
  brush bg= brush (false);
  b= cell_box (tb->ip, tb, 0, 0, x1, y1, x2, y2,
               lborder, rborder, bborder, tborder, fg, bg);
  SI Lsep= lsep+lborder, Rsep= rsep+rborder;
  SI Bsep= bsep+bborder, Tsep= tsep+tborder;
  if ((Lsep != 0) || (Rsep != 0) || (Bsep != 0) || (Tsep != 0))
    b= cell_box (b->ip, b, Lsep, 0, x1, y1-Bsep, x2+Lsep+Rsep, y2+Tsep,
                 0, 0, 0, 0, fg, bg);
}

array<box>
table_rep::var_finish () {
  if (hyphen == "n") {
    array<box> bs (1);
    finish ();
    bs[0]= b;
    return bs;
  }

  int i, j;
  array<box> stack;
  for (i=0; i<nr_rows; i++) {
    array<box> bs;
    array<SI>  x;
    array<SI>  y;
    for (j=0; j<nr_cols; j++)
      if (!is_nil (T[i][j])) {
        cell C = T[i][j];
        C->finish ();
        bs << C->b;
        x  << C->x1;
        y  << (C->y1 + C->shift);
      }
    if (N(bs)==0) continue;

    box   tb= composite_box (ip, bs, x, y, false);
    SI    BB= (i==(nr_rows-1)? bborder: 0);
    SI    TB= (i==0? tborder: 0);
    SI    BS= (i==(nr_rows-1)? bsep: 0);
    SI    TS= (i==0? tsep: 0);
    SI    x1= tb->x1 - lborder;
    SI    x2= tb->x2 + rborder;
    SI    y1= tb->y1 - BB;
    SI    y2= tb->y2 + TB;
    brush fg= env->pen->get_brush ();
    brush bg= brush (false);
    b= cell_box (tb->ip, tb, 0, 0, x1, y1, x2, y2,
                 lborder, rborder, BB, TB, fg, bg);
    SI Lsep= lsep+lborder, Rsep= rsep+rborder;
    SI Bsep= BS+BB, Tsep= TS+TB;
    if ((Lsep != 0) || (Rsep != 0) || (Bsep != 0) || (Tsep != 0))
      b= cell_box (b->ip, b, Lsep, 0, x1, y1-Bsep, x2+Lsep+Rsep, y2+Tsep,
                   0, 0, 0, 0, fg, bg);
    stack << b;
  }
  return stack;
}

/******************************************************************************
* Lazy tables
******************************************************************************/

struct lazy_table_rep: public lazy_rep {
  table T; // the table
  lazy_table_rep (table T2, path ip): lazy_rep (LAZY_TABLE, ip), T (T2) {}
  operator tree () { return "Table"; }
  lazy produce (lazy_type request, format fm);
  format query (lazy_type request, format fm);
};

struct lazy_table {
  EXTEND_NULL(lazy,lazy_table);
  inline lazy_table (table T, path ip):
    rep (tm_new<lazy_table_rep> (T, ip)) { rep->ref_count= 1; }
};
EXTEND_NULL_CODE(lazy,lazy_table);

format
lazy_table_rep::query (lazy_type request, format fm) {
  //cout << "Query= " << request << LF;
  //cout << "  format= " << fm << LF;
  //cout << "  par= " << T->env->as_length ("1par") << LF;
  if ((request == LAZY_BOX) && (fm->type == QUERY_VSTREAM_WIDTH)) {
    SI tmw, tlw, trw;
    T->compute_width (tmw, tlw, trw, true);
    return make_format_width (max (tmw, 1));
  }
  return lazy_rep::query (request, fm);
}

lazy
lazy_table_rep::produce (lazy_type request, format fm) {
  //cout << "produce= " << request << LF;
  //cout << "  format= " << fm << LF;
  //cout << "  par= " << T->env->as_length ("1par") << LF;
  if (request == type) return this;
  if (request == LAZY_VSTREAM) {
    if (fm->type == FORMAT_VSTREAM) {
      format_vstream fs= (format_vstream) fm;
      string s= as_string (T->var[TABLE_WIDTH]);
      if (ends (s, "par"))
        T->width= max ((SI) (as_double (s (0, N(s)-3)) * fs->width), 1);
    }
    T->position_columns (true);
    T->finish_horizontal ();
    T->position_rows ();
    array<box> bs= T->var_finish ();
    tree old1= T->env->local_begin (PAR_LEFT, "0tmpt");
    tree old2= T->env->local_begin (PAR_RIGHT, "0tmpt");
    // FIXME: check whether we should also set the other
    // paragraph formatting variables, as in cell_rep::typeset
    lazy tmp= make_lazy_paragraph (T->env, bs, ip);
    T->env->local_end (PAR_RIGHT, old2);
    T->env->local_end (PAR_LEFT, old1);
    return tmp->produce (request, fm);
  }
  return lazy_rep::produce (request, fm);
}

lazy
make_lazy_table (edit_env env, tree t, path ip) {
  table T (env);
  T->typeset (t, ip);
  T->handle_decorations ();
  T->handle_span ();
  T->merge_borders (); // FIXME: introduce merge_border variables
  return lazy_table (T, ip);
}

/******************************************************************************
* User interface
******************************************************************************/

box
typeset_as_table (edit_env env, tree t, path ip) {
  table T (env);
  T->typeset (t, ip);
  T->handle_decorations ();
  T->handle_span ();
  T->merge_borders ();
  T->position_columns (true);
  T->finish_horizontal ();
  T->position_rows ();
  T->finish ();
  return T->b;
}

array<box>
typeset_as_var_table (edit_env env, tree t, path ip) {
  table T (env);
  T->typeset (t, ip);
  T->handle_decorations ();
  T->handle_span ();
  T->merge_borders ();
  T->position_columns (true);
  T->finish_horizontal ();
  T->position_rows ();
  return T->var_finish ();
}
