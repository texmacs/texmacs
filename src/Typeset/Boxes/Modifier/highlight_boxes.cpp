
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
  SI w, xpad, ypad;
  brush bg, sunc, shad, old_bg;
  pencil old_pen;
  highlight_box_rep (path ip, box b, SI w, SI xpad, SI ypad,
		     brush bg, brush sunc, brush shad);
  operator tree () { return tree (TUPLE, "highlight", (tree) bs[0]); }
  void pre_display (renderer &ren);
  void post_display (renderer &ren);
  void display_classic (renderer& ren);
};

highlight_box_rep::highlight_box_rep (
  path ip, box b, SI w2, SI xp2, SI yp2,
  brush bg2, brush sunc2, brush shad2):
    change_box_rep (ip, true), w (w2), xpad (xp2), ypad (yp2),
    bg (bg2), sunc (sunc2), shad (shad2)
{
  insert (b, w + xpad, 0);
  position ();
  x1= b->x1;
  y1= b->y1 - w - ypad;
  x2= b->x2 + 2 * (w + xpad);
  y2= b->y2 + w + ypad;
  x3= min (x1, b->x3 + w + xpad);
  y3= min (y1, b->y3);
  x4= max (x2, b->x4 + w + xpad);
  y4= max (y2, b->y4);
  finalize ();
}

void
highlight_box_rep::pre_display (renderer& ren) {
  old_bg = ren->get_background ();
  old_pen= ren->get_pencil ();
  display_classic (ren);
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
  ren->set_pencil (pencil (sunc, 0));
  ren->fill (x1  , y2-W, x2  , y2  );
  ren->fill (x1  , y1  , x1+W, y2  );
  ren->set_pencil (pencil (shad, 0));
  ren->fill (x1+W, y1  , x2  , y1+W);
  ren->fill (x2-W, y1  , x2  , y2-W);
  ren->draw_triangle (x1, y1, x1+W, y1, x1+W, y1+W);
  ren->draw_triangle (x2, y2, x2, y2-W, x2-W, y2-W);
}

/******************************************************************************
* box construction routines
******************************************************************************/

box
highlight_box (path ip, box b, SI w, SI xpad, SI ypad,
	       brush bg, brush sunc, brush shad) {
  return tm_new<highlight_box_rep> (ip, b, w, xpad, ypad, bg, sunc, shad);
}
