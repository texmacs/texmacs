
/******************************************************************************
* MODULE     : highlight_boxes.cpp
* DESCRIPTION: Boxes with ornaments
* COPYRIGHT  : (C) 2013  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "Boxes/change.hpp"
#include "Boxes/construct.hpp"
#include "scheme.hpp"
#include "gui.hpp"
#include "effect.hpp"
#include "analyze.hpp"

#define ROUNDED_NORMAL   0
#define ROUNDED_ANGULAR  1
#define ROUNDED_CARTOON  2

/******************************************************************************
* Highlight boxes
******************************************************************************/

struct highlight_box_rep: public change_box_rep {
  tree shape;
  SI lw, bw, rw, tw;
  SI lx, bx, rx, tx;
  SI lpad, bpad, rpad, tpad;
  brush bg, xc;
  array<brush> bc;
  brush old_bg;
  pencil old_pen;
  highlight_box_rep (path ip, box b, box xb, ornament_parameters ps);
  operator tree () { return tree (TUPLE, "highlight", (tree) bs[0]); }
  void pre_display (renderer &ren);
  void post_display (renderer &ren);
  void display_classic (renderer& ren);
  void display_ring (renderer& ren);
  void display_band (renderer& ren);
  void display_rounded (renderer& ren, int style);
};

highlight_box_rep::highlight_box_rep (path ip, box b, box xb,
				      ornament_parameters ps):
  change_box_rep (ip, true), shape (ps->shape),
  lw (ps->lw), bw (ps->bw), rw (ps->rw), tw (ps->tw),
  lx ((SI) (ps->lext * lw)), bx ((SI) (ps->bext * bw)),
  rx ((SI) (ps->rext * rw)), tx ((SI) (ps->text * tw)),
  lpad (ps->lpad), bpad (ps->bpad), rpad (ps->rpad), tpad (ps->tpad),
  bg (ps->bg), xc (ps->xc), bc (ps->border)
{
  ASSERT (N(bc) == 4 || N(bc) == 8, "invalid number of border colors");
  if (shape == "ring" || shape == "band") {
    lpad= lpad + rpad;
    rpad= 0;
  }
  SI offx= 0, offy= 0;
  insert (b, lpad, 0);
  if (!is_nil (xb)) {
    offx= b->x1 - xb->x1;
    offy= b->y2 - xb->y1 + bpad + tpad;
    insert (xb, offx + lpad, offy);
  }
  position ();
  x1= b->x1;
  y1= b->y1 - bpad;
  x2= b->x2 + lpad + rpad;
  y2= b->y2 + tpad;
  x3= min (x1 - lx, b->x3 + lpad);
  y3= min (y1 - bx, b->y3);
  x4= max (x2 + rx, b->x4 + lpad);
  y4= max (y2 + tx, b->y4);
  if (!is_nil (xb)) {
    x1= min (x1, offx + xb->x1);
    y1= min (y1, offy + xb->y1 - bpad);
    x2= max (x2, offx + xb->x2 + lpad + rpad);
    y2= max (y2, offy + xb->y2 + tpad);
    x3= min (x3, min (x1, offx + xb->x3 + lpad));
    y3= min (y3, min (y1, offy + xb->y3));
    x4= max (x4, max (x2, offx + xb->x4 + lpad));
    y4= max (y4, max (y2, offy + xb->y4));
  }
  finalize ();
}

void
highlight_box_rep::pre_display (renderer& ren) {
  old_bg = ren->get_background ();
  old_pen= ren->get_pencil ();
  if (shape == "classic") display_classic (ren);
  else if (shape == "rounded") display_rounded (ren, ROUNDED_NORMAL);
  else if (shape == "angular") display_rounded (ren, ROUNDED_ANGULAR);
  else if (shape == "cartoon") display_rounded (ren, ROUNDED_CARTOON);
  else if (shape == "ring");
  else if (shape == "band");
  else display_classic (ren);
}

void
highlight_box_rep::post_display (renderer &ren) {
  if (shape == "ring") display_ring (ren);
  if (shape == "band") display_band (ren);
  ren->set_background (old_bg);
  ren->set_pencil (old_pen);
}

/******************************************************************************
* Classic ornaments
******************************************************************************/

void
highlight_box_rep::display_classic (renderer& ren) {
  SI X1= x1 - lx, Y1= y1 - bx;
  SI X2= x2 + rx, Y2= y2 + tx;
  SI LW= lw, BW= bw, RW= rw, TW= tw;
  if (!ren->is_printer ()) {
    SI pixel= ren->pixel;
    LW= ((lw + pixel - 1) / pixel) * pixel;
    BW= ((bw + pixel - 1) / pixel) * pixel;
    RW= ((rw + pixel - 1) / pixel) * pixel;
    TW= ((tw + pixel - 1) / pixel) * pixel;
  }
  ren->set_background (bg);
  ren->clear_pattern (X1+LW, Y1+BW, X2-RW, Y2-TW);
  if (N(bs)>1) {
    SI m= (sy2(0) + sy1(1)) >> 1;
    ren->set_background (xc);
    ren->clear_pattern (X1+LW, m, X2-RW, Y2-TW);    
  }
  if (N(bc) == 4) {
    ren->set_brush (bc[0]);
    ren->fill (X1   , Y1   , X1+LW, Y2   );
    ren->set_brush (bc[1]);
    ren->fill (X1+LW, Y1   , X2   , Y1+BW);
    ren->set_brush (bc[2]);
    ren->fill (X2-RW, Y1   , X2   , Y2-TW);
    ren->set_brush (bc[3]);  
    ren->fill (X1   , Y2-TW, X2   , Y2   );
    if (bc[0] != bc[1]) {
      ren->set_brush (bc[1]);
      ren->draw_triangle (X1, Y1, X1+LW, Y1   , X1+LW, Y1+BW);
    }
    if (bc[2] != bc[1]) {
      ren->set_brush (bc[1]);
      ren->draw_triangle (X2, Y1, X2-RW, Y1   , X2-RW, Y1+BW);
    }
    if (bc[2] != bc[3]) {
      ren->set_brush (bc[2]);
      ren->draw_triangle (X2, Y2, X2   , Y2-TW, X2-RW, Y2-TW);
    }
    if (bc[0] != bc[3]) {
      ren->set_brush (bc[0]);
      ren->draw_triangle (X1, Y2, X1   , Y2-TW, X1+LW, Y2-TW);
    }
    if (N(bs)>1 && TW>0 && bc[0] == bc[3] && bc[2] == bc[3]) {
      SI m= (sy2(0) + sy1(1)) >> 1;
      ren->set_brush (bc[3]);
      ren->set_pencil (pencil (bc[3], TW));
      ren->line (X1+LW, m, X2-RW, m);
    }
  }
  if (N(bc) == 8) {
    ren->set_brush (bc[0]);
    ren->fill (X1   , Y1+BW, X1+LW, Y2-TW);
    ren->set_brush (bc[1]);
    ren->fill (X1+LW, Y1   , X2-RW, Y1+BW);
    ren->set_brush (bc[2]);
    ren->fill (X2-RW, Y1+BW, X2   , Y2-TW);
    ren->set_brush (bc[3]);  
    ren->fill (X1+RW, Y2-TW, X2-RW, Y2   );
    ren->set_brush (bc[4]);
    ren->fill (X1   , Y1   , X1+LW, Y1+BW);
    ren->set_brush (bc[5]);
    ren->fill (X2-RW, Y1   , X2   , Y1+BW);
    ren->set_brush (bc[6]);
    ren->fill (X2-RW, Y2-TW, X2   , Y2   );
    ren->set_brush (bc[7]);
    ren->fill (X1   , Y2-TW, X1+LW, Y2   );
  }
}

/******************************************************************************
* Ring ornaments
******************************************************************************/

void
highlight_box_rep::display_ring (renderer& ren) {
  SI LW= lw, BW= bw, TW= tw;
  if (!ren->is_printer ()) {
    SI pixel= ren->pixel;
    LW= ((lw + pixel - 1) / pixel) * pixel;
    BW= ((bw + pixel - 1) / pixel) * pixel;
    TW= ((tw + pixel - 1) / pixel) * pixel;
  }

  ren->set_background (bg);
  if ((y1+2*BW) < (y2-2*TW))
    ren->clear_pattern (x1, y1+2*BW, x1 + LW + 2*lpad, y2-2*TW);
}

/******************************************************************************
* Band ornaments
******************************************************************************/

void
highlight_box_rep::display_band (renderer& ren) {
  SI X1= x1 - lx, Y1= y1 - bx;
  SI X2= x1 + lpad + rx, Y2= y2 + tx;
  SI LW= lw, BW= bw, RW= rw, TW= tw;
  if (!ren->is_printer ()) {
    SI pixel= ren->pixel;
    LW= ((lw + pixel - 1) / pixel) * pixel;
    BW= ((bw + pixel - 1) / pixel) * pixel;
    RW= ((rw + pixel - 1) / pixel) * pixel;
    TW= ((tw + pixel - 1) / pixel) * pixel;
  }
  ren->set_background (bg);
  ren->clear_pattern (X1+LW, Y1+BW, X2-RW, Y2-TW);
  if (N(bc) == 4) {
    ren->set_brush (bc[0]);
    ren->fill (X1   , Y1   , X1+LW, Y2   );
    ren->set_brush (bc[1]);
    ren->fill (X1+LW, Y1   , X2   , Y1+BW);
    ren->set_brush (bc[2]);
    ren->fill (X2-RW, Y1   , X2   , Y2-TW);
    ren->set_brush (bc[3]);  
    ren->fill (X1   , Y2-TW, X2   , Y2   );
    if (bc[0] != bc[1]) {
      ren->set_brush (bc[1]);
      ren->draw_triangle (X1, Y1, X1+LW, Y1   , X1+LW, Y1+BW);
    }
    if (bc[2] != bc[1]) {
      ren->set_brush (bc[1]);
      ren->draw_triangle (X2, Y1, X2-RW, Y1   , X2-RW, Y1+BW);
    }
    if (bc[2] != bc[3]) {
      ren->set_brush (bc[2]);
      ren->draw_triangle (X2, Y2, X2   , Y2-TW, X2-RW, Y2-TW);
    }
    if (bc[0] != bc[3]) {
      ren->set_brush (bc[0]);
      ren->draw_triangle (X1, Y2, X1   , Y2-TW, X1+LW, Y2-TW);
    }
  }
}

/******************************************************************************
* Rounded ornaments
******************************************************************************/

void
rounded (array<SI>& xs, array<SI>& ys,
         SI cx, SI cy, SI x1, SI y1, SI x2, SI y2,
         bool start, bool end, int style) {
  switch (style) {
  case ROUNDED_NORMAL:
  case ROUNDED_ANGULAR: {
    int n= (style == ROUNDED_NORMAL? 16: 1);
    for (int i=0; i<=n; i++)
      if ((i>0 || start) && (i<n || end)) {
        double a= (i * 1.57079632679) / n;
        if (x1 != cx && y2 != cy) {
          SI x= (SI) (cx + cos (a) * (x1 - cx));
          SI y= (SI) (cy + sin (a) * (y2 - cy));
          xs << x; ys << y;
        }
        else {
          SI x= (SI) (cx + sin (a) * (x2 - cx));
          SI y= (SI) (cy + cos (a) * (y1 - cy));
          xs << x; ys << y;
        }
      }
    }
    break;
  case ROUNDED_CARTOON: {
    int n= 16;
    for (int i=0; i<=n; i++)
      if ((i>0 || start) && (i<n || end)) {
        double a= (i * 1.57079632679) / n;
        if (x1 != cx && y2 != cy) {
          SI x= (SI) (cx + (1 - sin (a)) * (x1 - cx));
          SI y= (SI) (cy + (1 - cos (a)) * (y2 - cy));
          xs << x; ys << y;
        }
        else {
          SI x= (SI) (cx + (1 - cos (a)) * (x2 - cx));
          SI y= (SI) (cy + (1 - sin (a)) * (y1 - cy));
          xs << x; ys << y;
        }
      }
    }
    break;
  }
}

void
render_mixed (renderer ren, brush b1, brush b2,
              array<SI> xs, array<SI> ys) {
  SI n= min (N(xs), N(ys)) - 1;
  for (int i=0; i < n; i++) {
    double a= (2.0 * i + 1.0) / (2.0 * n);
    brush mb= mix (b1, 1.0 - a, b2, a);
    ren->set_brush (mb);
    ren->line (xs[i], ys[i], xs[i+1], ys[i+1]);    
  }
}

void
highlight_box_rep::display_rounded (renderer& ren, int style) {
  SI W   = max (max (lw, rw), max (bw, tw));
  SI xpad= min (lpad, rpad);
  SI ypad= min (bpad, tpad);
  SI Rx= (SI) (2 * xpad);
  SI Ry= (SI) (2 * ypad);
  if (style == ROUNDED_ANGULAR) {
    Rx= (SI) ((3 * xpad) / 2);
    Ry= (SI) ((3 * ypad) / 2);
  }
  if (style == ROUNDED_CARTOON) {
    Rx= (SI) xpad;
    Ry= (SI) ypad;
  }
  //if (!ren->is_printer ()) {
  //  SI pixel= ren->pixel;
  //  W= ((W + (2*pixel) - 1) / (2*pixel)) * (2*pixel);
  //}
  SI l1= x1 + (W>>1) - lx;
  SI l2= x1 + Rx;
  SI r1= x2 - (W>>1) + rx;
  SI r2= x2 - Rx;
  SI b1= y1 + (W>>1) - bx;
  SI b2= y1 + Ry;
  SI t1= y2 - (W>>1) + tx;
  SI t2= y2 - Ry;
  array<SI> xs, ys;
  rounded (xs, ys, l2, b2, l1, b2, l2, b1, true, true, style);
  rounded (xs, ys, r2, b2, r2, b1, r1, b2, true, true, style);
  rounded (xs, ys, r2, t2, r1, t2, r2, t1, true, true, style);
  rounded (xs, ys, l2, t2, l2, t1, l1, t2, true, true, style);
  if (bg->get_type() != brush_none) {
    ren->set_brush (bg);
    ren->polygon (xs, ys);
  }
  if (N(bs)>1 && xc->get_type() != brush_none) {
    SI m= (sy2(0) + sy1(1)) >> 1;
    array<SI> Xs, Ys;
    Xs << l1; Ys << m;
    Xs << r1; Ys << m;
    rounded (Xs, Ys, r2, t2, r1, t2, r2, t1, true, true, style);
    rounded (Xs, Ys, l2, t2, l2, t1, l1, t2, true, true, style);
    ren->set_brush (xc);
    ren->polygon (Xs, Ys);
  }
  if (W > 0) {
    ren->set_brush (bc[0]);
    ren->set_pencil (pencil (bc[0], W));
    xs << l1; ys << b2;
    ren->lines (xs, ys);
    if (N(bs)>1 && bc[0] == bc[1]) {
      SI m= (sy2(0) + sy1(1)) >> 1;
      ren->line (l1, m, r1, m);
    }
    if (N(bc) == 4 && bc[1] != bc[0]) {
      ren->set_brush (bc[1]);
      ren->line (l2, b1, r2, b1);
      array<SI> cxs, cys;
      rounded (cxs, cys, l2, b2, l1, b2, l2, b1, true, true, style);
      render_mixed (ren, bc[0], bc[1], cxs, cys);
    }
    if (N(bc) == 4 && bc[2] != bc[0]) {
      ren->set_brush (bc[2]);
      ren->line (r1, b2, r1, t2);
    }
    if (N(bc) == 4 && (bc[1] != bc[0] || bc[2] != bc[0])) {
      array<SI> cxs, cys;
      rounded (cxs, cys, r2, b2, r2, b1, r1, b2, true, true, style);
      render_mixed (ren, bc[1], bc[2], cxs, cys);
    }
    if (N(bc) == 4 && (bc[2] != bc[0] || bc[3] != bc[0])) {
      array<SI> cxs, cys;
      rounded (cxs, cys, r2, t2, r1, t2, r2, t1, true, true, style);
      render_mixed (ren, bc[2], bc[3], cxs, cys);
    }
    if (N(bc) == 4 && bc[3] != bc[0]) {
      ren->set_brush (bc[3]);
      ren->line (l2, t1, r2, t1);
      array<SI> cxs, cys;
      rounded (cxs, cys, l2, t2, l2, t1, l1, t2, true, true, style);
      render_mixed (ren, bc[3], bc[0], cxs, cys);
    }
  }
}

/******************************************************************************
* Putting titles by superposing several highlight boxes
******************************************************************************/

box
title_box (path ip, box b, box xb, ornament_parameters ps) {
  string tst= ps->tst->label;
  bool at_top   = starts (tst, "top ");
  bool at_bot   = starts (tst, "bottom ");
  bool at_left  = ends (tst, " left");
  bool at_center= ends (tst, " center");
  bool at_right = ends (tst, " right");

  ornament_parameters cps= copy (ps);
  cps->tst= "classic";
  ornament_parameters tit_ps= copy (cps);
  ornament_parameters bb_ps = copy (cps);
  tit_ps->bg= tit_ps->xc;
  bb_ps->xc = bb_ps->bg;

  if (!(at_top || at_bot) || !(at_left || at_center || at_right))
    return highlight_box (ip, b, xb, cps);
  box tit  = highlight_box (ip, xb, box (), tit_ps);
  SI  shift= tit->h() >> 1;
  box shb  = vresize_box (b->ip, b,
                          b->y1 - (at_bot? shift: 0),
                          b->y2 + (at_top? shift: 0));
  box bb   = highlight_box (ip, shb, box (), bb_ps);
  SI  tx   = ((bb->x1 + bb->x2) >> 1) - ((tit->x1 + tit->x2) >> 1);
  SI  ty   = (at_top? bb->y2: bb->y1) - tit->y1 - shift;
  if (at_left ) tx= bb->x1 - tit->x1 + ps->lw + 2 * ps->lpad;
  if (at_right) tx= bb->x2 - tit->x2 - ps->rw - 2 * ps->rpad;
  array<box> bs;
  array<SI>  x, y;
  bs << bb << tit;
  x << 0 << tx;
  y << 0 << ty;
  return composite_box (ip, bs, x, y, false);
  // FIXME: we should force redrawing the title box,
  // whenever redrawing the body box.
}

/******************************************************************************
* Box construction routines
******************************************************************************/

box
highlight_box (path ip, box b, box xb, ornament_parameters ps) {
  if (ps->tst != "classic" && !is_nil (xb) && is_atomic (ps->tst))
    return title_box (ip, b, xb, ps);
  return tm_new<highlight_box_rep> (ip, b, xb, ps);
}

box
highlight_box (path ip, box b, SI w, brush c, brush sunc, brush shad) {
  array<brush> border; border << sunc << shad << shad << sunc;
  ornament_parameters ps ("classic", "classic",
                          w, w, w, w,
			  0.0, 0.0, 0.0, 0.0,
			  0, 0, 0, 0,
                          c, c, border);
  return highlight_box (ip, b, box (), ps);
}
