
/******************************************************************************
* MODULE     : misc.cpp
* DESCRIPTION: Decoration boxes (mostly for use on the screen)
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "Boxes/composite.hpp"
#include "Boxes/construct.hpp"
#include "scheme.hpp"

bool in_presentation_mode ();

/******************************************************************************
* Specific boxes
******************************************************************************/

struct specific_box_rep: public box_rep {
  box b;
  string filter;
  specific_box_rep (path ip, box b2, string filter2, font fn, bool keep_size):
    box_rep (ip), b (b2), filter (filter2) {
      if (keep_size) {
        x1= b->x1; y1= b->y1;
        x2= b->x2; y2= b->y2;
      }
      else {
        x1=x2=y1=0;
        y2=fn->yx;
      }
      x3= b->x3; y3= b->y3;
      x4= b->x4; y4= b->y4;
    }
  operator tree () {
    return tuple ("specific", (tree) b, filter); }
  void display (renderer ren) {
    bool ok= false;
    if (filter == "screen") ok= !ren->is_printer () && !in_presentation_mode ();
    else if (filter == "printer") ok= ren->is_printer ();
    else if (filter == "even") ok= (ren->cur_page & 1) == 0;
    else if (filter == "odd") ok= (ren->cur_page & 1) == 1;
    if (ok) {
      rectangles rs;
      b->redraw (ren, path (), rs);
    }
  }
};

/******************************************************************************
* TOC boxes
******************************************************************************/

struct toc_box_rep: public box_rep {
  string kind, title;
  toc_box_rep (path ip, string kind2, string title2, font fn):
    box_rep (ip), kind (kind2), title (title2) {
      x1=x2=y1=0;
      y2=fn->yx;
      x3=x4=y3=y4= 0;
    }
  operator tree () {
    return tuple ("toc", kind, title); }
  void display (renderer ren) {
    ren->toc_entry (kind, title, 0, y2); }
};

/******************************************************************************
* Flag boxes
******************************************************************************/

struct flag_box_rep: public composite_box_rep {
  brush light;
  brush old_bg;
  flag_box_rep (path ip, box b, SI h, pencil dark, brush light);
  operator tree () { return tree (TUPLE, "flag"); }
  void pre_display (renderer &ren);
  void post_display (renderer &ren);
};

void
flag_box_rep::pre_display (renderer &ren) {
  old_bg= ren->get_background ();
  ren->set_background (light);
}

void
flag_box_rep::post_display (renderer &ren) {
  ren->set_background (old_bg);
}

flag_box_rep::flag_box_rep (
  path ip, box b, SI h, pencil dark, brush light2):
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
  insert (line_box (dip, 0, 0, 0, h, dark), 0, 0);
  insert (polygon_box (dip, x, y, light, dark), 0, h);
  insert (b, sep - b->x1, h + sep - b->y1);
  position ();
  finalize ();
}

/******************************************************************************
* Info boxes
******************************************************************************/

struct info_box_rep: public composite_box_rep {
  info_box_rep (path ip, SI h, pencil dark, brush light);
  operator tree () { return tree (TUPLE, "info"); }
};

info_box_rep::info_box_rep (path ip, SI h, pencil dark, brush light):
  composite_box_rep (ip)
{
  SI d= h/5;
  path dip= decorate_middle (ip);
  array<SI> x(4), y(4);
  x[0]= 0; y[0]= -d;
  x[1]= d; y[1]= 0;
  x[2]= 0; y[2]= d;
  x[3]= -d; y[3]= 0;
  insert (line_box (dip, 0, 0, 0, h-d, dark), 0, 0);
  insert (polygon_box (dip, x, y, light, dark), 0, h);
  position ();
  finalize ();
}

/******************************************************************************
* Scrollbar boxes
******************************************************************************/

struct scrollbar_box_rep: public composite_box_rep {
  bool vertical;
  SI span;
  tree t;
  scrollbar_box_rep (path ip, box b, bool vertical2, SI span2, tree t2):
    composite_box_rep (ip), vertical (vertical2), span (span2), t (t2) {
      insert (b, 0, 0); position (); finalize (); }
  operator tree () { return tuple ("scrollbar", (tree) bs[0]); }
  tree action (tree type, SI x, SI y, SI delta);
};

tree
scrollbar_box_rep::action (tree type, SI x, SI y, SI delta) {
  (void) type; (void) delta;
  tree u= t;
  if (vertical) {
    double p= 100.0;
    SI Y1= y1 + (span>>1);
    SI Y2= y2 - (span>>1);
    if (Y1 < Y2) p= 100.0 * ((double) (y-Y1)) / ((double) (Y2-Y1));
    p= min (100.0, max (0.0, p));
    u= tree (as_string (p) * "%");
  }
  else {
    double p= 0.0;
    SI X1= x1 + (span>>1);
    SI X2= x2 - (span>>1);
    if (X1 < X2) p= 100.0 * ((double) (x-X1)) / ((double) (X2-X1));
    p= max (0.0, min (100.0, p));
    u= tree (as_string (p) * "%");
  }
  if (u != t && is_accessible (obtain_ip (t)))
    {
      object fun= symbol_object ("tree-set");
      object cmd= list_object (fun, t, u);
      exec_delayed (scheme_cmd (cmd));
    }
  return "done";
}

/******************************************************************************
* box construction routines
******************************************************************************/

box
specific_box (path ip, box b, string filter, font fn, bool keep_size) {
  return tm_new<specific_box_rep> (ip, b, filter, fn, keep_size);
}

box
toc_box (path ip, string kind, string title, font fn) {
  return tm_new<toc_box_rep> (ip, kind, title, fn);
}

box
flag_box (path ip, box b, SI h, pencil dark, brush light) {
  return tm_new<flag_box_rep> (ip, b, h, dark, light);
}

box
info_box (path ip, SI h, pencil dark, brush light) {
  return tm_new<info_box_rep> (ip, h, dark, light);
}

box
scrollbar_box (path ip, box b, bool vertical, SI span, tree t) {
  return tm_new<scrollbar_box_rep> (ip, b, vertical, span, t);
}
