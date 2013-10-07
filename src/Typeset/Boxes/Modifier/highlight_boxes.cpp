
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

/******************************************************************************
* Highlight boxes
******************************************************************************/

struct highlight_box_rep: public change_box_rep {
  tree shape;
  SI w, xpad, ypad;
  brush bg, xc, sunc, shad, old_bg;
  pencil old_pen;
  highlight_box_rep (path ip, box b, box xb, tree shape,
		     SI w, SI xpad, SI ypad,
		     brush bg, brush xc, brush sunc, brush shad);
  operator tree () { return tree (TUPLE, "highlight", (tree) bs[0]); }
  void pre_display (renderer &ren);
  void post_display (renderer &ren);
  void display_classic (renderer& ren);
  void display_rounded (renderer& ren);
};

highlight_box_rep::highlight_box_rep (
  path ip, box b, box xb, tree shape2, SI w2, SI xp2, SI yp2,
  brush bg2, brush xc2, brush sunc2, brush shad2):
    change_box_rep (ip, true), shape (shape2),
    w (w2), xpad (xp2), ypad (yp2),
    bg (bg2), xc (xc2), sunc (sunc2), shad (shad2)
{
  SI offx= 0, offy= 0;
  insert (b, w + xpad, 0);
  if (!is_nil (xb)) {
    offx= b->x1 - xb->x1;
    offy= b->y2 - xb->y1 + w + 2*ypad;
    insert (xb, offx + w + xpad, offy);
  }
  position ();
  x1= b->x1;
  y1= b->y1 - w - ypad;
  x2= b->x2 + 2 * (w + xpad);
  y2= b->y2 + w + ypad;
  x3= min (x1, b->x3 + w + xpad);
  y3= min (y1, b->y3);
  x4= max (x2, b->x4 + w + xpad);
  y4= max (y2, b->y4);
  if (!is_nil (xb)) {
    x1= min (x1, offx + xb->x1);
    y1= min (y1, offy + xb->y1 - w - ypad);
    x2= max (x2, offx + xb->x2 + 2 * (w + xpad));
    y2= max (y2, offy + xb->y2 + w + ypad);
    x3= min (x3, min (x1, offx + xb->x3 + w + xpad));
    y3= min (y3, min (y1, offy + xb->y3));
    x4= max (x4, max (x2, offx + xb->x4 + w + xpad));
    y4= max (y4, max (y2, offy + xb->y4));
  }
  finalize ();
}

void
highlight_box_rep::pre_display (renderer& ren) {
  old_bg = ren->get_background ();
  old_pen= ren->get_pencil ();
  if (shape == "classic") display_classic (ren);
  else if (shape == "rounded") display_rounded (ren);
  else display_classic (ren);
}

void
highlight_box_rep::post_display (renderer &ren) {
  ren->set_background (old_bg);
  ren->set_pencil (old_pen);
}

/******************************************************************************
* Classic ornaments
******************************************************************************/

void
highlight_box_rep::display_classic (renderer& ren) {
  SI W= w;
  if (!ren->is_printer ()) {
    SI pixel= ren->pixel;
    W= ((w + pixel - 1) / pixel) * pixel;
  }
  ren->set_background (bg);
  ren->clear_pattern (x1+W, y1+W, x2-W, y2-W);
  if (N(bs)>1) {
    SI m= (sy2(0) + sy1(1)) >> 1;
    ren->set_background (xc);
    ren->clear_pattern (x1+W, m, x2-W, y2-W);    
  }
  ren->set_brush (sunc);
  ren->fill (x1  , y2-W, x2  , y2  );
  ren->fill (x1  , y1  , x1+W, y2  );
  ren->set_brush (shad);
  ren->fill (x1+W, y1  , x2  , y1+W);
  ren->fill (x2-W, y1  , x2  , y2-W);
  if (sunc != shad) {
    ren->draw_triangle (x1, y1, x1+W, y1, x1+W, y1+W);
    ren->draw_triangle (x2, y2, x2, y2-W, x2-W, y2-W);
  }
}

/******************************************************************************
* Rounded ornaments
******************************************************************************/

void
rounded (array<SI>& xs, array<SI>& ys,
         SI cx, SI cy, SI x1, SI y1, SI x2, SI y2,
         bool start, bool end) {
  const int n= 16;
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

void
highlight_box_rep::display_rounded (renderer& ren) {
  SI W = w;
  SI Rx= (SI) (2 * (xpad-W));
  SI Ry= (SI) (2 * (ypad-W));
  if (!ren->is_printer ()) {
    SI pixel= ren->pixel;
    W= ((w + (2*pixel) - 1) / (2*pixel)) * (2*pixel);
  }
  SI l1= x1+(W>>1);
  SI l2= x1+Rx;
  SI r1= x2-(W>>1);
  SI r2= x2-Rx;
  SI b1= y1+(W>>1);
  SI b2= y1+Ry;
  SI t1= y2-(W>>1);
  SI t2= y2-Ry;
  array<SI> xs, ys;
  rounded (xs, ys, l2, b2, l1, b2, l2, b1, true, true);
  rounded (xs, ys, r2, b2, r2, b1, r1, b2, true, true);
  rounded (xs, ys, r2, t2, r1, t2, r2, t1, true, true);
  rounded (xs, ys, l2, t2, l2, t1, l1, t2, true, true);
  if (bg->get_type() != brush_none) {
    ren->set_brush (bg);
    ren->polygon (xs, ys);
  }
  if (N(bs)>1 && xc->get_type() != brush_none) {
    SI m= (sy2(0) + sy1(1)) >> 1;
    array<SI> Xs, Ys;
    Xs << l1; Ys << m;
    Xs << r1; Ys << m;
    rounded (Xs, Ys, r2, t2, r1, t2, r2, t1, true, true);
    rounded (Xs, Ys, l2, t2, l2, t1, l1, t2, true, true);
    ren->set_brush (xc);
    ren->polygon (Xs, Ys);
  }
  if (W > 0) {
    ren->set_brush (sunc);
    ren->set_pencil (pencil (sunc, W));
    xs << l1; ys << b2;
    ren->lines (xs, ys);
  }
}

/******************************************************************************
* box construction routines
******************************************************************************/

box
highlight_box (path ip, box b, box xb, tree shape,
               SI w, SI xpad, SI ypad,
	       brush bg, brush xc, brush sunc, brush shad) {
  return tm_new<highlight_box_rep> (ip, b, xb, shape,
                                    w, xpad, ypad, bg, xc, sunc, shad);
}
