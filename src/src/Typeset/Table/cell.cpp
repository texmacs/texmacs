
/******************************************************************************
* MODULE     : cell.cpp
* DESCRIPTION: Cells of tables
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "Format/format.hpp"
#include "Table/table.hpp"
#include "Boxes/construct.hpp"

/******************************************************************************
* Cells
******************************************************************************/

cell_rep::cell_rep (edit_env env2): var (""), env (env2) {}

void
cell_rep::typeset (tree fm, tree t, path iq) {
  ip= iq;
  format_cell (fm);
  if (is_func (t, CELL, 1)) {
    iq= descend (iq, 0);
    t = t[0];
  }

  if (is_func (t, SUBTABLE, 1)) {
    lsep= rsep= bsep= tsep= 0;
    T= table (env, 2);
    T->typeset (t[0], descend (iq, 0));
  }
  else {
    if (hyphen == "n") {
      b= typeset_as_concat (env, t, iq);
      if (vcorrect != "n") {
	SI y1= b->y1;
	SI y2= b->y2;
	if ((vcorrect == "a") || (vcorrect == "b")) y1= min (y1, env->fn->y1);
	if ((vcorrect == "a") || (vcorrect == "t")) y2= max (y2, env->fn->y2);
	b= resize_box (iq, b, b->x1, y1, b->x2, y2);
      }
    }
    else {
      b= empty_box (iq);

      tree old1= env->local_begin (PAGE_MEDIUM, "papyrus");
      tree old2= env->local_begin (PAR_LEFT, "0unit");
      tree old3= env->local_begin (PAR_RIGHT, "0unit");
      tree old4= env->local_begin (PAR_MODE, "justify");
      tree old5= env->local_begin (PAR_NO_FIRST, "true");
  
      lz= make_lazy (env, t, iq);
      
      env->local_end (PAR_NO_FIRST, old5);
      env->local_end (PAR_MODE, old4);
      env->local_end (PAR_RIGHT, old3);
      env->local_end (PAR_LEFT, old2);
      env->local_end (PAGE_MEDIUM, old1);
    }
  }

  if (decoration != "") {
    int i, j, or_row= -1, or_col= -1;
    tree dt= decoration;
    while (is_func (dt, TFORMAT)) dt= dt [N(dt)-1];
    for (i=0; i<N(dt); i++) {
      tree dr= dt[i];
      while (is_func (dr, TFORMAT)) dr= dr [N(dr)-1];
      for (j=0; j<N(dr); j++) {
	tree dc= dc[j];
	while (is_func (dc, TFORMAT)) dc= dc [N(dc)-1];
	if (dc == tree (TMARKER)) {
	  or_row= i;
	  or_col= j;
	}
      }
    }

    if (or_row != -1) {
      D= table (env, 1, or_row, or_col);
      D->typeset (decoration, decorate (iq));
    }
  }
}

/******************************************************************************
* Formatting routines
******************************************************************************/

void
extract_format (tree fm, tree* r, int n) {
  int i;
  for (i=0; i<n; i++) r[i]= tree (TFORMAT);
  if (!is_func (fm, TFORMAT)) return;
  for (i=0; i<N(fm); i++)
    if (is_func (fm[i], CWITH))
      if ((N(fm[i]) >= 2) &&
	  (is_int (fm[i][0])) &&
	  (is_int (fm[i][1])))
	{
	  int k;
	  int k1= as_int (fm[i][0]);
	  int k2= as_int (fm[i][1]);
	  tree u= fm[i] (2, N (fm[i]));
	  if (k1>=0) k1--; else k1+=n;
	  if (k2> 0) k2--; else k2+=n;
	  if ((k1 >= n) || (k2 < 0)) continue;
	  k1= max (k1, 0);
	  k2= min (k2, n-1);
	  for (k=k1; k<=k2; k++) r[k] << u;
	}
}

void
cell_rep::format_cell (tree fm) {
  int i, l= N(fm);
  for (i=0; i<l; i++)
    format_item (fm[i]);

  if (var->contains (CELL_DECORATION))
    decoration= env->exec (var[CELL_DECORATION]);
  else decoration= "";
  if (var->contains (CELL_BACKGROUND)) {
    bg= as_string (env->exec (var[CELL_BACKGROUND]));
    if (bg == "foreground") bg= env->get_string (COLOR);
  }
  else bg= "";
  if (var->contains (CELL_WIDTH)) {
    width= env->decode_length (env->exec (var[CELL_WIDTH]));
    if (var->contains (CELL_HMODE))
      hmode= as_string (env->exec (var[CELL_HMODE]));
    else hmode= "exact";
  }
  else {
    width= 0;
    hmode= "";
  }
  if (var->contains (CELL_HEIGHT)) {
    height= env->decode_length (env->exec (var[CELL_HEIGHT]));
    if (var->contains (CELL_VMODE))
      vmode= as_string (env->exec (var[CELL_VMODE]));
    else vmode= "exact";
  }
  else {
    height= 0;
    vmode = "";
  }
  if (var->contains (CELL_HPART))
    hpart= as_double (env->exec (var[CELL_HPART]));
  else hpart= 0.0;
  if (var->contains (CELL_VPART))
    vpart= as_double (env->exec (var[CELL_VPART]));
  else vpart= 0.0;
  if (var->contains (CELL_LSEP))
    lsep= env->decode_length (env->exec (var[CELL_LSEP]));
  else lsep= env->fn->spc->def;
  if (var->contains (CELL_RSEP))
    rsep= env->decode_length (env->exec (var[CELL_RSEP]));
  else rsep= env->fn->spc->def;
  if (var->contains (CELL_BSEP))
    bsep= env->decode_length (env->exec (var[CELL_BSEP]));
  else bsep= env->fn->sep;
  if (var->contains (CELL_TSEP))
    tsep= env->decode_length (env->exec (var[CELL_TSEP]));
  else tsep= env->fn->sep;
  if (var->contains (CELL_LBORDER))
    lborder= env->decode_length (env->exec (var[CELL_LBORDER])) >> 1;
  else lborder= 0;
  if (var->contains (CELL_RBORDER))
    rborder= env->decode_length (env->exec (var[CELL_RBORDER])) >> 1;
  else rborder= 0;
  if (var->contains (CELL_BBORDER))
    bborder= env->decode_length (env->exec (var[CELL_BBORDER])) >> 1;
  else bborder= 0;
  if (var->contains (CELL_TBORDER))
    tborder= env->decode_length (env->exec (var[CELL_TBORDER])) >> 1;
  else tborder= 0;
  if (var->contains (CELL_HALIGN))
    halign= as_string (env->exec (var[CELL_HALIGN]));
  else halign= "l";
  if (var->contains (CELL_VALIGN))
    valign= as_string (env->exec (var[CELL_VALIGN]));
  else valign= "B";
  if (var->contains (CELL_VCORRECT))
    vcorrect= as_string (env->exec (var[CELL_VCORRECT]));
  else vcorrect= "a";
  if (var->contains (CELL_HYPHEN))
    hyphen= as_string (env->exec (var[CELL_HYPHEN]));
  else hyphen= "n";
  if (var->contains (CELL_ROW_SPAN))
    row_span= as_int (env->exec (var[CELL_ROW_SPAN]));
  else row_span= 1;
  if (var->contains (CELL_COL_SPAN))
    col_span= as_int (env->exec (var[CELL_COL_SPAN]));
  else col_span= 1;
}

void
cell_rep::format_item (tree with) {
  if (is_func (with, CWITH, 2))
    var (as_string (with[0]))= with[1];
}

/******************************************************************************
* Positioning routines
******************************************************************************/

void
cell_rep::compute_width (SI& mw, SI& lw, SI& rw, bool large) {
  if (!nil (T)) {
    T->compute_width (mw, lw, rw);
    if ((T->halign == "L") || (T->halign == "C") ||
	(T->halign == "R") || (T->halign == "O"))
      {
	lw += lborder;
	rw += rborder;
      }
    else lw= rw= 0;
    mw += lborder + rborder;
  }
  else if (large && (!nil (lz))) {
    lw= rw= 0;
    format fm= lz->query (LAZY_BOX, make_query_vstream_width (0, 0));
    format_width fw= (format_width) fm;
    mw= fw->width + lsep + rsep + lborder + rborder;
  }
  else {
    lw= rw= mw= 0;
    if ((halign == "L") || (halign == "C") || (halign == "R")) {
      lw= -b->x1+ lsep + lborder;
      rw=  b->x2+ rsep + rborder;
      mw=  lw + rw;
    }
    else if ((halign == ".") || (halign == ",")) {
      SI offset= b->get_leaf_offset (halign);
      lw= offset+ lsep+ lborder;
      rw= b->w()- offset+ rsep+ rborder;
      mw= lw + rw;
    }
    else mw= b->w() + lsep + rsep + lborder + rborder;
  }

  if (hmode == "exact") { // "BUG": handle L, C, R differently
    SI d= (mw - width) >> 1;
    lw -= d; rw -= d; mw= width;
  }
  else if (hmode == "max") mw= max (width, mw);
  else if (hmode == "min") { // "BUG": handle L, C, R differently
    SI d= (mw - min (width, mw)) >> 1;
    lw -= d; rw -= d; mw= min (width, mw);
  }
}

void
cell_rep::compute_height (SI& mh, SI& bh, SI& th) {
  if (nil (T)) {
    bh= th= mh= 0;
    if ((valign == "B") || (valign == "C") || (valign == "T")) {
      bh= -b->y1+ bsep + bborder;
      th=  b->y2+ tsep + tborder;
      mh=  bh + th;
    }
    else mh= b->h() + bsep + tsep + bborder + tborder;
  }
  else {
    T->compute_height (mh, bh, th);
    if ((T->valign == "B") || (T->valign == "C") ||
	(T->valign == "T") || (T->valign == "O"))
      {
	bh += bborder;
	th += tborder;
      }
    else bh= th= 0;
    mh += bborder + tborder;
  }

  if (vmode == "exact") { // "BUG": handle B, C, T differently
    SI d= (mh - height) >> 1;
    bh -= d; th -= d; mh= height;
  }
  else if (vmode == "max") mh= max (height, mh);
  else if (vmode == "min") { // "BUG": handle B, C, T differently
    SI d= (mh - min (height, mh)) >> 1;
    bh -= d; th -= d; mh= min (height, mh);
  }
}

void
cell_rep::fit_horizontally () {
  SI  w= width- lsep- lborder- rsep- rborder;
  int v= hyphen == "t"? 1: (hyphen == "c"? 0: -1);
  SI  d= ((vcorrect == "b") || (vcorrect == "a"))? -env->fn->y1: 0;
  SI  h= ((vcorrect == "t") || (vcorrect == "a"))?  env->fn->y2: 0;
  b= (box) lz->produce (LAZY_BOX, make_format_cell (w, v, d, h));
}

void
cell_rep::position_horizontally (SI offset, SI mw, SI lw, SI rw) {
  x1= offset;
  x2= offset+ mw;
  if (nil (T)) {
    if (halign == "l") xoff= -b->x1 + (lsep + lborder);
    else if (halign == "c") xoff= (mw- b->x1- b->x2) >> 1;
    else if (halign == "r") xoff= mw- b->x2- (rsep + rborder);
    else if (halign == "L") xoff= lw;
    else if (halign == "C") xoff= (mw+ lw- rw) >> 1;
    else if (halign == "R") xoff= mw- rw;
    else if (halign == ".") xoff= lw- b->get_leaf_offset (halign);
    else if (halign == ",") xoff= lw- b->get_leaf_offset (halign);
    else xoff= lw;
  }
  else xoff= -T->x1+ lborder;
}

void
cell_rep::position_vertically (SI offset, SI mh, SI bh, SI th) {
  y1= offset- mh;
  y2= offset;
  if (nil (T)) {
    if (valign == "b") yoff= -b->y1+ (bsep + bborder);
    else if (valign == "c") yoff= (mh- b->y1- b->y2) >> 1;
    else if (valign == "t") yoff= mh- b->y2- (tsep + tborder);
    else if (valign == "B") yoff= bh;
    else if (valign == "C") yoff= (mh+ bh- th) >> 1;
    else if (valign == "T") yoff= mh- th;
    else yoff= bh;
  }
  else yoff= -T->y1+ bborder;
}

/******************************************************************************
* Producing the final cell box
******************************************************************************/

void
cell_rep::finish () {
  if (!nil (T)) {
    T->finish ();
    b= T->b;
  }

  color fc= env->col;
  bool  tr= (bg == "");
  color bc= (tr? env->dis->white: env->dis->get_color (bg));

  b= cell_box (ip, b, xoff, yoff, 0, 0, x2-x1, y2-y1,
	       lborder, rborder, bborder, tborder, fc, bc, tr);
}
