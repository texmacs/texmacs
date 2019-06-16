
/******************************************************************************
* MODULE     : basic.hpp
* DESCRIPTION: Basic boxes are the most elementary boxes.
*              We have implemented the following ones:
*                - a test box
*                - line boxes
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "boxes.hpp"
#include "formatter.hpp"

/******************************************************************************
* The test box
******************************************************************************/

struct test_box_rep: public box_rep {
  test_box_rep (path ip): box_rep (ip) {
    x1=x3=0; x2=x4=50<<8; y1=y3=0; y2=y4= 25<<8; }
  operator tree () { return "test"; }
  void display (renderer ren);
};

void
test_box_rep::display (renderer ren) {
  ren->set_pencil (pencil (green, PIXEL));
  ren->line (x1, y1, x2, y2);
  ren->line (x1, y2, x2, y1);
  // ren->arc (x1, y1, x2, y2, 45*64, 90*64);
}

/******************************************************************************
* Line boxes
******************************************************************************/

struct line_box_rep: public box_rep {
  SI X1, Y1, X2, Y2;
  pencil pen;

  line_box_rep (path ip, SI X1b, SI Y1b, SI X2b, SI Y2b, pencil pen);
  operator tree () { return "line"; }
  void display (renderer ren);
};

line_box_rep::line_box_rep (path ip, SI X1b, SI Y1b, SI X2b, SI Y2b, pencil p):
  box_rep (ip)
{
  SI w = p->get_width ();
  X1 = X1b;
  Y1 = Y1b;
  X2 = X2b;
  Y2 = Y2b;
  pen= p;
  x1 = min (X1, X2); y1 = min (Y1, Y2);
  x2 = max (X1, X2); y2 = max (Y1, Y2);
  x3 = x1-(w>>1);    y3 = y1-(w>>1); 
  x4 = x2+(w>>1);    y4 = y2+(w>>1);
}

void
line_box_rep::display (renderer ren) {
  ren->set_pencil (pen);
  ren->line (X1, Y1, X2, Y2);
}

/******************************************************************************
* Polygon boxes
******************************************************************************/

struct polygon_box_rep: public box_rep {
  array<SI> x, y;
  brush fill;
  pencil outline;

  polygon_box_rep (path ip, array<SI> x, array<SI> y, brush f, pencil o);
  operator tree () { return "polygon"; }
  void display (renderer ren);
};

polygon_box_rep::polygon_box_rep (
  path ip, array<SI> X, array<SI> Y, brush f, pencil o):
    box_rep (ip), x (X), y (Y), fill (f), outline (o)
{
  SI w= outline->get_width ();
  int i, n= N(x);
  x1= x2= x[0]; y1= y2= y[0];
  for (i=1; i<n; i++) {
    x1= min (x1, x[i]); y1= min (y1, y[i]);
    x2= max (x2, x[i]); y2= max (y2, y[i]);
  }
  x3= x1-(w>>1); y3= y1-(w>>1); 
  x4= x2+(w>>1); y4= y2+(w>>1);
}

void
polygon_box_rep::display (renderer ren) {
  ren->set_pencil (pencil (fill, 0));
  ren->polygon (x, y);
  if (outline->get_width () > 0) {
    int i, n= N(x);
    ren->set_pencil (outline);
    for (i=0; i<n; i++)
      ren->line (x[i], y[i], x[(i+1)%n], y[(i+1)%n]);
  }
}

/******************************************************************************
* Arc boxes
******************************************************************************/

struct arc_box_rep: public box_rep {
  SI  X1, Y1, X2, Y2;
  int a1, a2;
  pencil pen;

  arc_box_rep (path ip, SI X1b, SI Y1b, SI X2b, SI Y2b,
	       int A1, int A2, pencil pen);
  operator tree () { return "arc"; }
  void display (renderer ren);
};

arc_box_rep::arc_box_rep (path ip, SI X1b, SI Y1b, SI X2b, SI Y2b,
			  int a1b, int a2b, pencil p): box_rep (ip)
{
  X1 = X1b;
  Y1 = Y1b;
  X2 = X2b;
  Y2 = Y2b;
  a1 = a1b;
  a2 = a2b;
  pen= p;

  double scale= 3.1415927 / (180<<6);
  SI P1= ((X1+X2)/2) + (SI) (((X2-X1)/2) * cos (((double) a1) * scale));
  SI Q1= ((Y1+Y2)/2) + (SI) (((Y2-Y1)/2) * sin (((double) a1) * scale));
  SI P2= ((X1+X2)/2) + (SI) (((X2-X1)/2) * cos (((double) a2) * scale));
  SI Q2= ((Y1+Y2)/2) + (SI) (((Y2-Y1)/2) * sin (((double) a2) * scale));
  SI p1= min (P1, P2);
  SI q1= min (Q1, Q2);
  SI p2= max (P1, P2);
  SI q2= max (Q1, Q2);

  int s= (a1>>6)%360;
  int d= ((a2-a1)>>6);
  if ((s< 90) && ((s+d)> 90)) q2= Y2;
  if ((s<180) && ((s+d)>180)) p1= X1;
  if ((s<270) && ((s+d)>270)) q1= Y1;
  if ((s<360) && ((s+d)>360)) p2= X2;
  if ((s<450) && ((s+d)>450)) q2= Y2;
  if ((s<540) && ((s+d)>540)) p1= X1;
  if ((s<630) && ((s+d)>630)) q1= Y1;

  SI w = pen->get_width ();
  x1= min (p1, p2); y1= min (q1, q2);
  x2= max (p1, p2); y2= max (q1, q2);
  x3= x1-(w>>1);    y3= y1-(w>>1); 
  x4= x2+(w>>1);    y4= y2+(w>>1);
}

void
arc_box_rep::display (renderer ren) {
  ren->set_pencil (pen);
  ren->arc (X1, Y1, X2, Y2, a1, a2-a1);
  // ren->line (x1, y1, x2, y2);
}

/******************************************************************************
* Image boxes
******************************************************************************/

struct image_box_rep: public box_rep {
  scalable im;
  int alpha;
  image_box_rep (path ip, scalable im, int alpha);
  operator tree () { return "image"; }
  void display (renderer ren);
};

image_box_rep::image_box_rep (path ip, scalable im2, int alpha2):
  box_rep (ip), im (im2), alpha (alpha2)
{
  rectangle rl= im->get_logical_extents ();
  rectangle rp= im->get_physical_extents ();
  x1= rl->x1; y1= rl->y1;
  x2= rl->x2; y2= rl->y2;
  x3= rp->x1; y3= rp->y1;
  x4= rp->x2; y4= rp->y2;
}

void
image_box_rep::display (renderer ren) {
  ren->draw_scalable (im, 0, 0, alpha);
}

/******************************************************************************
* Control boxes
******************************************************************************/

struct control_tree_box_rep: public box_rep {
  tree t;
  control_tree_box_rep (path ip, tree t2, font fn): box_rep (ip), t (t2) {
    x1=x2=x3=x4=y1=y3=y4=0; y2=fn->yx; }
  operator tree () { return tuple ("control tree", (tree) t); }
  void display (renderer ren) { (void) ren; }
  tree get_leaf_tree () { return t; }
};

struct control_box_box_rep: public box_rep {
  box b;
  control_box_box_rep (path ip, box b2, font fn): box_rep (ip), b (b2) {
    x1=x2=x3=x4=y1=y3=y4=0; y2=fn->yx; }
  operator tree () { return tuple ("control box", (tree) b); }
  void display (renderer ren) { (void) ren; }
  box get_leaf_box () { return b; }
};

struct control_lazy_box_rep: public box_rep {
  lazy lz;
  control_lazy_box_rep (path ip, lazy lz2, font fn): box_rep (ip), lz (lz2) {
    x1=x2=x3=x4=y1=y3=y4=0; y2=fn->yx; }
  operator tree () { return tuple ("control lazy", (tree) lz); }
  void display (renderer ren) { (void) ren; }
  lazy get_leaf_lazy () { return lz; }
};

/******************************************************************************
* box construction routines
******************************************************************************/

box
test_box (path ip) {
  return tm_new<test_box_rep> (ip);
}

box
line_box (path ip, SI x1, SI y1, SI x2, SI y2, pencil pen) {
  return tm_new<line_box_rep> (ip, x1, y1, x2, y2, pen);
}

box
arc_box (path ip, SI x1, SI y1, SI x2, SI y2, int a1, int a2, pencil pen) {
  return tm_new<arc_box_rep> (ip, x1, y1, x2, y2, a1, a2, pen);
}

box
polygon_box (path ip, array<SI> x, array<SI> y, brush fill, pencil outline) {
  return tm_new<polygon_box_rep> (ip, x, y, fill, outline);
}

box
polygon_box (path ip, array<SI> x, array<SI> y, brush fill) {
  return tm_new<polygon_box_rep> (ip, x, y, fill, pencil (fill, 0));
}

box
image_box (path ip, url u, SI w, SI h, int alpha, int px) {
  scalable im= load_scalable_image (u, w, h, "", px);
  return tm_new<image_box_rep> (ip, im, alpha);
}

box
control_box (path ip, tree t, font fn) {
  return tm_new<control_tree_box_rep> (ip, t, fn);
}

box
control_box (path ip, box b, font fn) {
  return tm_new<control_box_box_rep> (ip, b, fn);
}

box
control_box (path ip, lazy lz, font fn) {
  return tm_new<control_lazy_box_rep> (ip, lz, fn);
}
