
/******************************************************************************
* MODULE     : cell.cpp
* DESCRIPTION: Cells of tables
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "Format/format.hpp"
#include "Table/table.hpp"
#include "Boxes/construct.hpp"

/******************************************************************************
* Cells
******************************************************************************/

cell_rep::cell_rep (edit_env env2):
  var (""), env (env2), border_flags (0) {}

void
cell_rep::typeset (tree fm, tree t, path iq) {
  ip= iq;
  format_cell (fm);
  if (is_func (t, CELL, 1)) {
    iq= descend (iq, 0);
    t = t[0];
  }

  cell_local_begin (fm);
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
        b= vresize_box (iq, b, y1, y2);
      }
      if (swell > 0) swell_padding ();
    }
    else {
      b= empty_box (iq);

      tree len = env->as_tmlen ("1par");
      tree old1= env->local_begin (PAGE_MEDIUM, "papyrus");
      tree old2= env->local_begin (PAR_LEFT, "0tmpt");
      tree old3= env->local_begin (PAR_RIGHT, "0tmpt");
      tree old4= env->local_begin (PAR_MODE, "justify");
      tree old5= env->local_begin (PAR_NO_FIRST, "true");
      //tree old6= env->local_begin (PAR_COLUMNS, "1");
      tree old7= env->local_begin (PAR_WIDTH, len);

      lz= make_lazy (env, t, iq);
      
      env->local_end (PAR_WIDTH, old7);
      //env->local_end (PAR_COLUMNS, old6);
      env->local_end (PAR_NO_FIRST, old5);
      env->local_end (PAR_MODE, old4);
      env->local_end (PAR_RIGHT, old3);
      env->local_end (PAR_LEFT, old2);
      env->local_end (PAGE_MEDIUM, old1);
    }
  }
  cell_local_end (fm);

  if (decoration != "") {
    int i, j, or_row= -1, or_col= -1;
    tree dt= decoration;
    while (is_func (dt, TFORMAT)) dt= dt [N(dt)-1];
    for (i=0; i<N(dt); i++) {
      tree dr= dt[i];
      while (is_func (dr, TFORMAT)) dr= dr [N(dr)-1];
      for (j=0; j<N(dr); j++) {
        tree dc= dr[j];
        while (is_func (dc, TFORMAT)) dc= dc [N(dc)-1];
        if (dc == tree (TMARKER)) {
          or_row= i;
          or_col= j;
        }
      }
    }

    if (or_row != -1) {
      D= table (env, 1, or_row, or_col);
      D->typeset (attach_deco (decoration, iq));
    }
  }
}

void
cell_rep::cell_local_begin (tree fm) {
  int i, l= N(fm);
  for (i=0; i<l; i++) {
    tree with= fm[i];
    if (is_func (with, CWITH, 2) &&
        is_atomic (with[0]) &&
        !starts (with[0]->label, "cell-")) {
      tree old= env->local_begin (with[0]->label, with[1]);
      string v= with[0]->label * "-" * as_string (i);
      var (v)= old;
    }
  }
}

void
cell_rep::cell_local_end (tree fm) {
  int i, l= N(fm);
  for (i=l-1; i>=0; i--) {
    tree with= fm[i];
    if (is_func (with, CWITH, 2) &&
        is_atomic (with[0]) &&
        !starts (with[0]->label, "cell-")) {
      string v= with[0]->label * "-" * as_string (i);
      tree old= var [v];
      env->local_end (with[0]->label, old);
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
    bg= env->exec (var[CELL_BACKGROUND]);
    if (bg == "foreground") bg= env->get_string (COLOR);
  }
  else bg= "";
  if (var->contains (CELL_WIDTH)) {
    width= env->as_length (env->exec (var[CELL_WIDTH]));
    if (var->contains (CELL_HMODE))
      hmode= as_string (env->exec (var[CELL_HMODE]));
    else hmode= "exact";
  }
  else {
    width= 0;
    hmode= "auto";
  }
  if (var->contains (CELL_HEIGHT)) {
    height= env->as_length (env->exec (var[CELL_HEIGHT]));
    if (var->contains (CELL_VMODE))
      vmode= as_string (env->exec (var[CELL_VMODE]));
    else vmode= "exact";
  }
  else {
    height= 0;
    vmode = "auto";
  }
  if (var->contains (CELL_HPART))
    hpart= as_double (env->exec (var[CELL_HPART]));
  else hpart= 0.0;
  if (var->contains (CELL_VPART))
    vpart= as_double (env->exec (var[CELL_VPART]));
  else vpart= 0.0;
  if (var->contains (CELL_LSEP))
    lsep= env->as_length (env->exec (var[CELL_LSEP]));
  else lsep= env->fn->spc->def;
  if (var->contains (CELL_RSEP))
    rsep= env->as_length (env->exec (var[CELL_RSEP]));
  else rsep= env->fn->spc->def;
  if (var->contains (CELL_BSEP))
    bsep= env->as_length (env->exec (var[CELL_BSEP]));
  else bsep= env->fn->sep;
  if (var->contains (CELL_TSEP))
    tsep= env->as_length (env->exec (var[CELL_TSEP]));
  else tsep= env->fn->sep;
  if (var->contains (CELL_LBORDER))
    lborder= env->as_length (env->exec (var[CELL_LBORDER])) >> 1;
  else lborder= 0;
  if (var->contains (CELL_RBORDER))
    rborder= env->as_length (env->exec (var[CELL_RBORDER])) >> 1;
  else rborder= 0;
  if (var->contains (CELL_BBORDER))
    bborder= env->as_length (env->exec (var[CELL_BBORDER])) >> 1;
  else bborder= 0;
  if (var->contains (CELL_TBORDER))
    tborder= env->as_length (env->exec (var[CELL_TBORDER])) >> 1;
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
  if (var->contains (CELL_SWELL))
    swell= env->as_length (env->exec (var[CELL_SWELL])) >> 1;
  else swell= 0;
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
  //cout << "  large= " << large << "\n";
  char align_c= '\0'; if (N (halign) != 0) align_c= halign[0];
  bool lr_flag= is_upcase (align_c);
  if (!is_nil (T)) {
    if (N (T->halign) > 0) align_c= T->halign[0];
    else align_c= '\0';
    lr_flag= is_upcase (align_c);
    T->compute_width (mw, lw, rw);
    if (lr_flag) {
      lw += lborder;
      rw += rborder;
    }
    else lw= rw= 0;
    mw += lborder + rborder;
  }
  else if (!is_nil (lz) && large) {
    lw= rw= 0;
    format fm= lz->query (LAZY_BOX, make_query_vstream_width (0, 0));
    format_width fw= (format_width) fm;
    mw= fw->width + lsep + rsep + lborder + rborder;
    if (lr_flag) {
      lw= lsep + lborder;
      rw= fw->width + rsep + rborder;
    }
  }
  else {
    //cout << "  b= " << b << "\n";
    lw= rw= mw= 0;
    if (lr_flag) {
      if (N (halign) == 1) {
        lw= -b->x1+ lsep + lborder;
        rw=  b->x2+ rsep + rborder;
        mw=  lw + rw;
      }
      else {
        SI offset= b->get_leaf_offset (halign (1, N (halign)));
        lw= offset+ lsep+ lborder;
        rw= b->w()- offset+ rsep+ rborder;
        mw= lw + rw;
      }
    }
    else mw= b->w() + lsep + rsep + lborder + rborder;
  }

  if (hmode == "exact") mw= width;
  else if (hmode == "max") mw= max (width, mw);
  else if (hmode == "min") mw= min (width, mw);
  if (mw < lw + rw) {
    SI d= lw + rw - mw;
    if (align_c == 'L' || align_c == 'O') rw -= d;
    else if (align_c == 'C') { lw -= (d >> 1); rw -= ((d+1) >> 1); }
    else lw -= d;
  }
}

void
cell_rep::compute_height (SI& mh, SI& bh, SI& th) {
  char align_c= '\0'; if (N (valign) != 0) align_c= valign[0];
  if (is_nil (T)) {
    bh= th= mh= 0;
    if (is_upcase (align_c)) {
      bh= -b->y1+ bsep + bborder;
      th=  b->y2+ tsep + tborder;
      mh=  bh + th;
    }
    else mh= b->h() + bsep + tsep + bborder + tborder;
  }
  else {
    if (N (T->valign) != 0) align_c= T->valign[0];
    T->compute_height (mh, bh, th);
    if (is_upcase (align_c)) {
      bh += bborder;
      th += tborder;
    }
    else bh= th= 0;
    mh += bborder + tborder;
  }

  if (vmode == "exact") mh= height;
  else if (vmode == "max") mh= max (height, mh);
  else if (vmode == "min") mh= min (height, mh);
  if (mh < bh + th) {
    SI d= bh + th - mh;
    if (align_c == 'B' || align_c == 'O') th -= d;
    else if (align_c == 'C') { bh -= (d >> 1); th -= ((d+1) >> 1); }
    else bh -= d;
  }
}

void
cell_rep::position_horizontally (SI offset, SI mw, SI lw, SI rw) {
  x1= offset;
  x2= offset + mw;
  if (is_nil (T)) {
    char align_c= '\0'; if (N (halign) != 0) align_c= halign[0];
    if (align_c == 'l') xoff= -b->x1 + (lsep + lborder);
    else if (align_c == 'c') xoff= (mw - b->x1 - b->x2) >> 1;
    else if (align_c == 'r') xoff= mw - b->x2 - (rsep + rborder);
    else if (align_c == 'L') xoff= lw;
    else if (align_c == 'C') xoff= (mw + lw - rw) >> 1;
    else if (align_c == 'R') xoff= mw - rw;
    else xoff= lw;
    if (N(halign) > 1) xoff -= b->get_leaf_offset (halign (1, N (halign)));
  }
  else xoff= -T->x1 + lborder;
}

void
cell_rep::position_vertically (SI offset, SI mh, SI bh, SI th) {
  y1= offset - mh;
  y2= offset;
  if (is_nil (T)) {
    char align_c= '\0'; if (N (valign) != 0) align_c= valign[0];
    if (align_c == 'b') yoff= -b->y1 + (bsep + bborder);
    else if (align_c == 'c') yoff= (mh - b->y1 - b->y2) >> 1;
    else if (align_c == 't') yoff= mh - b->y2 - (tsep + tborder);
    else if (align_c == 'B') yoff= bh;
    else if (align_c == 'C') yoff= (mh + bh - th) >> 1;
    else if (align_c == 'T') yoff= mh - th;
    else yoff= bh;
  }
  else yoff= -T->y1 + bborder;
}

/******************************************************************************
* Producing the final cell box
******************************************************************************/

void
cell_rep::swell_padding () {
  if (row_span > 1) return;
  SI swt= env->get_length (MATH_TOP_SWELL_START);
  SI swb= env->get_length (MATH_BOT_SWELL_START);
  if (b->y2 > swt && (border_flags & 1) == 0) {
    SI swT= env->get_length (MATH_TOP_SWELL_END);
    double exceed= b->y2 - swt;
    double unit  = max (swT - swt, 1);
    double ratio = min (exceed / unit, 1.0);
    tsep += (SI) (ratio * swell);
  }
  if (b->y1 < swb && (border_flags & 2) == 0) {
    SI swB= env->get_length (MATH_BOT_SWELL_END);
    double exceed= swb - b->y1;
    double unit  = max (swb - swB, 1);
    double ratio = min (exceed / unit, 1.0);
    bsep += (SI) (ratio * swell);
  }
  swell= 0;
}

void
cell_rep::finish_horizontal () {
  SI  w= width- lsep- lborder- rsep- rborder;
  int v= hyphen == "t"? 1: (hyphen == "c"? 0: -1);
  SI  d= ((vcorrect == "b") || (vcorrect == "a"))? -env->fn->y1: 0;
  SI  h= ((vcorrect == "t") || (vcorrect == "a"))?  env->fn->y2: 0;
  b= (box) lz->produce (LAZY_BOX, make_format_cell (w, v, d, h));
  if (swell > 0) swell_padding ();
}

void
cell_rep::finish () {
  if (!is_nil (T)) {
    T->finish ();
    b= T->b;
  }

  b= cell_box (ip, b, xoff, yoff, 0, 0, x2-x1, y2-y1,
               lborder, rborder, bborder, tborder,
               env->pen->get_brush (), brush (bg, env->alpha));
}
