
/******************************************************************************
* MODULE     : table.cpp
* DESCRIPTION: Tables and cells of tables
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "Table/table.hpp"
#include "Boxes/construct.hpp"
#include "Format/format.hpp"

lazy make_lazy_paragraph (edit_env env, array<box> bs, path ip);

/******************************************************************************
* Tables
******************************************************************************/

table_rep::table_rep (edit_env env2, int status2, int i0b, int j0b):
  var (""), env (env2), status (status2), i0 (i0b), j0 (j0b),
  T (NULL), nr_rows (0), width (0), height (0) {}

table_rep::~table_rep () {
  if (T != NULL) {
    int i;
    for (i=0; i<nr_rows; i++) delete[] T[i];
    delete[] T;
  }
}

void
table_rep::display (bool flag) {
  int i, j;
  if (flag) cout << "---------------------------------------------------------------------------\n";
  else cout << "{ ";
  for (i=0; i<nr_rows; i++) {
    cout << "[ ";
    for (j=0; j<nr_cols; j++)
      if (!nil (T[i][j])) {
	cell C= T[i][j];
	if (j != 0) cout << ", ";
	if (!nil (C->b)) cout << C->b;
	else if (!nil (C->T)) {
	  cout << "subtable ";
	  C->T->display (false);
	}
	else cout << "nil";
	if (!nil (C->D)) {
	  cout << " & decoration ";
	  C->D->display (false);
	}
      }
    cout << " ]";
    if (flag) cout << "\n";
    else if (i<nr_rows-1) cout << ", ";
  }
  if (flag) cout << "---------------------------------------------------------------------------\n";
  else cout << " }";
}

void
table_rep::typeset (tree t, path iq) {
  ip= iq;
  tree old_format= env->local_begin (CELL_FORMAT, tree (TABLE_FORMAT));
  tree new_format= old_format;
  if (!is_func (new_format, TABLE_FORMAT)) new_format= tree (TABLE_FORMAT);
  while (is_func (t, TABLE_FORMAT)) {
    new_format= join (new_format, t (0, N(t)-1));
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
  T= new cell*[nr_rows];
  STACK_NEW_ARRAY (subformat, tree, nr_rows);
  extract_format (fm, subformat, nr_rows);
  for (i=0; i<nr_rows; i++)
    typeset_row (i, subformat[i], t[i], descend (ip, i));
  STACK_DELETE_ARRAY (subformat);
}

void
table_rep::typeset_row (int i, tree fm, tree t, path ip) {
  int j;
  nr_cols= N(t);
  T[i]= new cell[nr_cols];
  STACK_NEW_ARRAY (subformat, tree, nr_cols);
  extract_format (fm, subformat, nr_cols);
  for (j=0; j<nr_cols; j++) {
    cell& C= T[i][j];
    C= cell (env);
    C->typeset (subformat[j], t[j], descend (ip, j));
    C->row_span= min (C->row_span, nr_rows- i);
    C->col_span= min (C->col_span, nr_cols- j);
    if (hyphen == "y") C->row_span= 1;
  }
  STACK_DELETE_ARRAY (subformat);
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
    width= env->decode_length (env->exec (var[TABLE_WIDTH]));
    if (var->contains (TABLE_HMODE))
      hmode= as_string (env->exec (var[TABLE_HMODE]));
    else hmode= "exact";
  }
  else {
    width= 0;
    hmode= "";
  }
  if (var->contains (TABLE_HEIGHT)) {
    height= env->decode_length (env->exec (var[TABLE_HEIGHT]));
    if (var->contains (TABLE_VMODE))
      vmode= as_string (env->exec (var[TABLE_VMODE]));
    else vmode= "exact";
  }
  else {
    height= 0;
    vmode = "";
  }
  if (var->contains (TABLE_LSEP))
    lsep= env->decode_length (env->exec (var[TABLE_LSEP]));
  else lsep= 0;
  if (var->contains (TABLE_RSEP))
    rsep= env->decode_length (env->exec (var[TABLE_RSEP]));
  else rsep= 0;
  if (var->contains (TABLE_BSEP))
    bsep= env->decode_length (env->exec (var[TABLE_BSEP]));
  else bsep= 0;
  if (var->contains (TABLE_TSEP))
    tsep= env->decode_length (env->exec (var[TABLE_TSEP]));
  else tsep= 0;
  if (var->contains (TABLE_LBORDER))
    lborder= env->decode_length (env->exec (var[TABLE_LBORDER])) >> 1;
  else lborder= 0;
  if (var->contains (TABLE_RBORDER))
    rborder= env->decode_length (env->exec (var[TABLE_RBORDER])) >> 1;
  else rborder= 0;
  if (var->contains (TABLE_BBORDER))
    bborder= env->decode_length (env->exec (var[TABLE_BBORDER])) >> 1;
  else bborder= 0;
  if (var->contains (TABLE_TBORDER))
    tborder= env->decode_length (env->exec (var[TABLE_TBORDER])) >> 1;
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
  else row_origin= 0;
  if (var->contains (TABLE_COL_ORIGIN))
    col_origin= as_int (env->exec (var[TABLE_COL_ORIGIN]));
  else col_origin= 0;
}

void
table_rep::format_item (tree with) {
  if (is_func (with, TABLE_WITH, 2))
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
      if ((!nil (C)) && (!nil (C->T))) C->T->handle_decorations ();
      if ((!nil (C)) && (!nil (C->D))) {
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
  U= new cell*[new_rows];
  for (i=0; i<new_rows; i++)
    U[i]= new cell[new_cols];

  for (i=0; i<nr_rows; i++)
    for (j=0; j<nr_cols; j++) {
      cell C= T[i][j];
      if (!nil (C)) {
	if ((!nil (C->D)) && (C->D->status==1)) {
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
  for (i=0; i<nr_rows; i++) delete[] T[i];
  delete[] T;
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
      if (!nil (C)) {
	for (ii=0; ii<C->row_span; ii++)
	  for (jj=0; jj<C->col_span; jj++)
	    if ((ii != 0) || (jj != 0))
	      T[i+ii][j+jj]= cell ();
	if (!nil (C->T)) C->T->handle_span ();
      }
    }
}

/******************************************************************************
* Each border is the maximum of the borders of its adjacent cells
******************************************************************************/

void
table_rep::merge_borders () {
  int i1, j1;

  for (i1=0; i1<nr_rows; i1++)
    for (j1=0; j1<(nr_cols-1); j1++) {
      cell C1= T[i1][j1], C2;
      if (!nil (C1)) {
	int i2, j2= j1 + C1->col_span;
	if (j2 >= nr_cols) continue;
	for (i2=i1; i2>=0; i2--) {
	  C2= T[i2][j2];
	  if (!nil (C2)) break;
	}
	if (!nil (C2)) {
	  SI width= max (C1->rborder, C2->lborder);
	  C1->rborder= C2->lborder= width;
	  // ATTENTION: introduce new border variables when cells become lazy
	}
      }
    }

  for (i1=0; i1<(nr_rows-1); i1++)
    for (j1=0; j1<nr_cols; j1++) {
      cell C1= T[i1][j1], C2;
      if (!nil (C1)) {
	int i2= i1 + C1->row_span, j2;
	if (i2 >= nr_rows) continue;
	for (j2=j1; j2>=0; j2--) {
	  C2= T[i2][j2];
	  if (!nil (C2)) break;
	}
	if (!nil (C2)) {
	  SI width= max (C1->bborder, C2->tborder);
	  C1->bborder= C2->tborder= width;
	  // ATTENTION: introduce new border variables when cells become lazy
	}
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
blow_up (SI* w, SI* W, SI room, double* part, int n) {
  int i;
  double total= sum (part, n);
  if (total <= 0) {
    for (i=0; i<n; i++) part[i]= 1;
    total= n;
  }
  STACK_NEW_ARRAY (Part, double, n);
  for (i=0; i<n; i++) Part[i]= part[i];

  SI old_room= room;
  while (true) {
    for (i=0; i<n; i++)
      if (W[i] > w[i]) {
	SI extra= (SI) ((part[i]/total) * old_room);
	if (w[i]+extra >= W[i]) {
	  room    -= (W[i]-w[i]);
	  w[i]     = W[i];
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
    SI extra= (SI) ((part[i]/total) * room);
    w[i] += extra;
  }
  STACK_DELETE_ARRAY (Part);
}

/******************************************************************************
* Horizontal positioning
******************************************************************************/

void
table_rep::compute_width (SI& tmw, SI& tlw, SI& trw) {
  position_columns ();
  tmw= x2 - x1;
  tlw= -x1;
  trw= x2;
}

void
table_rep::compute_widths (SI* mw, SI* lw, SI* rw, bool large) {
  int i, j;
  for (j=0; j<nr_cols; j++)
    mw[j]= lw[j]= rw[j]= 0;

  for (j=0; j<nr_cols; j++)
    for (i=0; i<nr_rows; i++) {
      cell C= T[i][j];
      if ((!nil (C)) && (C->col_span==1)) {
	SI cmw, clw, crw;
	C->compute_width (cmw, clw, crw, large);
	mw[j]= max (mw[j], cmw);
	lw[j]= max (lw[j], clw);
	rw[j]= max (rw[j], crw);
	mw[j]= max (mw[j], lw[j] + rw[j]);
      }
    }

  for (j=0; j<nr_cols; j++)
    for (i=0; i<nr_rows; i++) {
      cell C= T[i][j];
      if ((!nil (C)) && (C->col_span!=1)) {
	SI cmw, clw, crw;
	C->compute_width (cmw, clw, crw, large);
	SI tot= sum (mw+j, C->col_span);
	if (cmw > tot) mw[j] += cmw - tot;
      }
    }
}

void
table_rep::compute_horizontal_parts (double* part) {
  int i, j;
  for (j=0; j<nr_cols; j++) part[j]= 0.0;
  for (j=0; j<nr_cols; j++)
    for (i=0; i<nr_rows; i++) {
      cell C= T[i][j];
      if (!nil (C))
	part[j]= max (part[j], C->hpart);
    }
}

void
table_rep::position_columns () {
  STACK_NEW_ARRAY (mw, SI, nr_cols);
  STACK_NEW_ARRAY (lw, SI, nr_cols);
  STACK_NEW_ARRAY (rw, SI, nr_cols);
  compute_widths (mw, lw, rw, false);
  if (width != 0) {
    SI hextra= max (width- sum (mw, nr_cols), 0);
    if (hextra > 0) {
      STACK_NEW_ARRAY (part, double, nr_cols);
      STACK_NEW_ARRAY (Mw, SI, nr_cols);
      STACK_NEW_ARRAY (Lw, SI, nr_cols);
      STACK_NEW_ARRAY (Rw, SI, nr_cols);
      compute_horizontal_parts (part);
      compute_widths (Mw, Lw, Rw, true);
      blow_up (mw, Mw, hextra, part, nr_cols);
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
      if (!nil (C)) {
	if (C->hyphen != "n") {
	  C->width= mw[j];
	  C->fit_horizontally ();
	}
	if (!nil (C->T)) {
	  C->T->width= mw[j]- C->lborder- C->rborder;
	  C->T->position_columns ();
	}
      }
    }

  SI xoff;
  if (halign == "l") xoff= 0;
  else if (halign == "c") xoff= -sum (mw, nr_cols) >> 1;
  else if (halign == "r") xoff= -sum (mw, nr_cols);
  else if (halign == "L") xoff= -lw[0];
  else if (halign == "C")
    xoff= -(sum (mw, nr_cols>>1)+ sum (mw, (nr_cols-1)>>1)+
	    lw[nr_cols>>1]+ lw[(nr_cols-1)>>1]) >> 1;
  else if (halign == "R") xoff= -sum (mw, nr_cols-1)- lw[nr_cols-1];
  else xoff= -sum (mw, j0)- lw[j0];

  x1= xoff- lborder- lsep;
  for (j=0; j<nr_cols; j++) {
    for (i=0; i<nr_rows; i++) {
      cell C= T[i][j];
      if (!nil (C)) {
	SI tot= sum (mw+j, C->col_span);
	C->position_horizontally (xoff, tot, lw[j], rw[j+C->col_span-1]);
      }
    }
    xoff += mw[j];
  }
  x2= xoff+ rborder+ rsep;
  STACK_DELETE_ARRAY (mw);
  STACK_DELETE_ARRAY (lw);
  STACK_DELETE_ARRAY (rw);
}

/******************************************************************************
* Vertical positioning
******************************************************************************/

void
table_rep::compute_height (SI& tmh, SI& tbh, SI& tth) {
  position_rows ();
  tmh= y2 - y1;
  tbh= -y1;
  tth= y2;
}

void
table_rep::compute_heights (SI* mh, SI* bh, SI* th) {
  int i, j;
  for (i=0; i<nr_rows; i++)
    mh[i]= bh[i]= th[i]= 0;

  for (i=0; i<nr_rows; i++)
    for (j=0; j<nr_cols; j++) {
      cell C= T[i][j];
      if ((!nil (C)) && (C->row_span==1)) {
	SI cmh, cbh, cth;
	C->compute_height (cmh, cbh, cth);
	mh[i]= max (mh[i], cmh);
	bh[i]= max (bh[i], cbh);
	th[i]= max (th[i], cth);
	mh[i]= max (mh[i], bh[i] + th[i]);
      }
    }

  for (i=0; i<nr_rows; i++)
    for (j=0; j<nr_cols; j++) {
      cell C= T[i][j];
      if ((!nil (C)) && (C->row_span!=1)) {
	SI cmh, cbh, cth;
	C->compute_height (cmh, cbh, cth);
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
      if (!nil (C))
	part[i]= max (part[i], C->vpart);
    }
}

void
table_rep::position_rows () {
  STACK_NEW_ARRAY (mh, SI, nr_rows);
  STACK_NEW_ARRAY (bh, SI, nr_rows);
  STACK_NEW_ARRAY (th, SI, nr_rows);

  compute_heights (mh, bh, th);
  if (height != 0) {
    SI vextra= max (height- sum (mh, nr_rows), 0);
    if (vextra > 0) {
      int i;
      STACK_NEW_ARRAY (part, double, nr_rows);
      STACK_NEW_ARRAY (Mh, SI, nr_rows);
      compute_vertical_parts (part);
      for (i=0; i<nr_rows; i++) Mh[i]= mh[i];
      blow_up (mh, Mh, vextra, part, nr_rows);
      STACK_DELETE_ARRAY (part);
      STACK_DELETE_ARRAY (Mh);
    }
  }

  int i, j;
  for (i=0; i<nr_rows; i++) {
    for (j=0; j<nr_cols; j++) {
      cell C= T[i][j];
      if ((!nil (C)) && (!nil (C->T))) {
	C->T->height= mh[j]- C->bborder- C->tborder;
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
    yoff= (sum (mh, nr_rows>>1)+ sum (mh, (nr_rows-1)>>1)+
	   th[nr_rows>>1]+ th[(nr_rows-1)>>1]) >> 1;
  else if (valign == "B") yoff= sum (mh, nr_rows-1)+ th[nr_rows-1];
  else yoff= sum (mh, i0)+ th[i0];

  y2= yoff+ tborder+ tsep;
  for (i=0; i<nr_rows; i++) {
    for (j=0; j<nr_cols; j++) {
      cell C= T[i][j];
      if (!nil (C)) {
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
}

/******************************************************************************
* Generate table
******************************************************************************/

void
table_rep::finish () {
  int i, j;
  array<box> bs;
  array<SI>  x;
  array<SI>  y;
  for (i=0; i<nr_rows; i++)
    for (j=0; j<nr_cols; j++)
      if (!nil (T[i][j])) {
	cell C = T[i][j];
	C->finish ();
	bs << C->b;
	x  << C->x1;
	y  << C->y1;
      }

  box   tb = composite_box (ip, bs, x, y, false);
  SI    x1= tb->x1;
  SI    y1= tb->y1;
  SI    x2= tb->x2;
  SI    y2= tb->y2;
  color fg= env->col;
  b= cell_box (tb->ip, tb, 0, 0, x1, y1, x2, y2,
	       lborder, rborder, bborder, rborder, fg, fg, true);
  SI Lsep= lsep+lborder, Rsep= rsep+rborder;
  SI Bsep= bsep+bborder, Tsep= tsep+tborder;
  if ((Lsep != 0) || (Rsep != 0) || (Bsep != 0) || (Tsep != 0))
    b= cell_box (b->ip, b, Lsep, 0, x1, y1-Bsep, x2+Lsep+Rsep, y2+Tsep,
		 0, 0, 0, 0, fg, fg, true);
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
      if (!nil (T[i][j])) {
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
    color fg= env->col;
    b= cell_box (tb->ip, tb, 0, 0, x1, y1, x2, y2,
		 lborder, rborder, BB, TB, fg, fg, true);
    SI Lsep= lsep+lborder, Rsep= rsep+rborder;
    SI Bsep= BS+BB, Tsep= TS+TB;
    if ((Lsep != 0) || (Rsep != 0) || (Bsep != 0) || (Tsep != 0))
      b= cell_box (b->ip, b, Lsep, 0, x1, y1-Bsep, x2+Lsep+Rsep, y2+Tsep,
		   0, 0, 0, 0, fg, fg, true);
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
    rep (new lazy_table_rep (T, ip)) { rep->ref_count= 1; }
};
EXTEND_NULL_CODE(lazy,lazy_table);

format
lazy_table_rep::query (lazy_type request, format fm) {
  if ((request == LAZY_BOX) && (fm->type == QUERY_VSTREAM_WIDTH)) {
    /* The following is still bugged
    SI tmw, tlw, trw;
    T->compute_width (tmw, tlw, trw); */
    SI tmw= 1;
    return make_format_width (tmw);
  }
  return lazy_rep::query (request, fm);
}

lazy
lazy_table_rep::produce (lazy_type request, format fm) {
  if (request == type) return this;
  if (request == LAZY_VSTREAM) {
    if (fm->type == FORMAT_VSTREAM) {
      format_vstream fs= (format_vstream) fm;
      if (T->var[TABLE_WIDTH] == "1par")
	T->width= fs->width;
    }
    T->merge_borders ();
    T->position_columns ();
    T->position_rows ();
    array<box> bs= T->var_finish ();
    lazy tmp= make_lazy_paragraph (T->env, bs, ip);
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
  T->position_columns ();
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
  T->position_columns ();
  T->position_rows ();
  return T->var_finish ();
}
