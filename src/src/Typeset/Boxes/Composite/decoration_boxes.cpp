
/******************************************************************************
* MODULE     : misc.cpp
* DESCRIPTION: Decoration boxes (mostly for use on the screen)
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "Boxes/composite.hpp"
#include "Boxes/construct.hpp"

/******************************************************************************
* Specific boxes
******************************************************************************/

struct specific_box_rep: public box_rep {
  box b;
  int type;
  specific_box_rep (path ip, box b2, int type2, font fn):
    box_rep (ip), b (b2), type (type2) {
      x1=x2=y1=0;
      y2=fn->yx;
      x3= b->x3; y3= b->y3;
      x4= b->x4; y4= b->y4;
    }
  operator tree () { return tuple ("specific", (tree) b, as_string (type)); }
  void display (ps_device dev) {
    if (dev->get_type () == type) {
      rectangles rs;
      b->redraw (dev, path (), rs);
    }
  }
};

/******************************************************************************
* Flag boxes
******************************************************************************/

struct flag_box_rep: public composite_box_rep {
  color light, old_bg;
  flag_box_rep (path ip, box b, SI h, SI lw, color dark, color light);
  operator tree () { return tree (TUPLE, "flag"); }
  void pre_display (ps_device &dev);
  void post_display (ps_device &dev);
};

void
flag_box_rep::pre_display (ps_device &dev) {
  old_bg= dev->get_background ();
  dev->set_background (light);
}

void
flag_box_rep::post_display (ps_device &dev) {
  dev->set_background (old_bg);
}

flag_box_rep::flag_box_rep (
  path ip, box b, SI h, SI lw, color dark, color light2):
  composite_box_rep (ip), light (light2)
{
  SI sep= h/5, H= b->h() + 2*sep, w= b->w() + 2*sep, W= H/4;
  path dip= decorate_middle (ip);
  array<SI> x(5), y(5);
  x[0]= 0; y[0]= 0;
  x[1]= w; y[1]= 0;
  x[2]= w+W; y[2]= H/2;
  x[3]= w; y[3]= H;
  x[4]= 0; y[4]= H;
  insert (line_box (dip, 0, 0, 0, h, lw, dark), 0, 0);
  insert (polygon_box (dip, x, y, lw, light, dark), 0, h);
  insert (b, sep - b->x1, h + sep - b->y1);
  position ();
  finalize ();
}

/******************************************************************************
* Info boxes
******************************************************************************/

struct info_box_rep: public composite_box_rep {
  info_box_rep (path ip, SI h, SI lw, color dark, color light);
  operator tree () { return tree (TUPLE, "info"); }
};

info_box_rep::info_box_rep (
  path ip, SI h, SI lw, color dark, color light):
  composite_box_rep (ip)
{
  SI d= h/5;
  path dip= decorate_middle (ip);
  array<SI> x(4), y(4);
  x[0]= 0; y[0]= -d;
  x[1]= d; y[1]= 0;
  x[2]= 0; y[2]= d;
  x[3]= -d; y[3]= 0;
  insert (line_box (dip, 0, 0, 0, h-d, lw, dark), 0, 0);
  insert (polygon_box (dip, x, y, lw, light, dark), 0, h);
  position ();
  finalize ();
}

/******************************************************************************
* box construction routines
******************************************************************************/

box
specific_box (path ip, box b, int type, font fn) {
  return new specific_box_rep (ip, b, type, fn);
}

box flag_box (path ip, box b, SI h, SI lw, color dark, color light) {
  return new flag_box_rep (ip, b, h, lw, dark, light);
}

box info_box (path ip, SI h, SI lw, color dark, color light) {
  return new info_box_rep (ip, h, lw, dark, light);
}

box
flag_box (path ip, string s, font fn, color dark, color light) {
  box b= text_box (decorate_right (ip), 0, s, fn, dark);
  return flag_box (ip, b, fn->wfn, fn->wline, dark, light);
}
