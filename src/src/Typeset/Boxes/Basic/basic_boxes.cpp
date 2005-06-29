
/******************************************************************************
* MODULE     : basic.hpp
* DESCRIPTION: Basic boxes are the most elementary boxes.
*              We have implemented the following ones:
*                - a test box
*                - line boxes
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
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
  void display (ps_device dev);
};

void
test_box_rep::display (ps_device dev) {
  dev->set_color (dev->green);
  dev->set_line_style (PIXEL);
  dev->line (x1, y1, x2, y2);
  dev->line (x1, y2, x2, y1);
  // dev->arc (x1, y1, x2, y2, 45*64, 90*64);
}

/******************************************************************************
* Line boxes
******************************************************************************/

struct line_box_rep: public box_rep {
  SI    X1, Y1, X2, Y2;
  SI    width;
  color col;

  line_box_rep (path ip, SI X1b, SI Y1b, SI X2b, SI Y2b, SI w, color c);
  operator tree () { return "line"; }
  void display (ps_device dev);
};

line_box_rep::line_box_rep (
  path ip, SI X1b, SI Y1b, SI X2b, SI Y2b, SI w, color c):
    box_rep (ip)
{
  X1     = X1b;
  Y1     = Y1b;
  X2     = X2b;
  Y2     = Y2b;
  width  = w;
  col    = c;
  x1= min (X1, X2); y1= min (Y1, Y2);
  x2= max (X1, X2); y2= max (Y1, Y2);
  x3= x1-(w>>1);    y3= y1-(w>>1); 
  x4= x2+(w>>1);    y4= y2+(w>>1);
}

void
line_box_rep::display (ps_device dev) {
  dev->set_line_style (width);
  dev->set_color (col);
  dev->line (X1, Y1, X2, Y2);
}

/******************************************************************************
* Polygon boxes
******************************************************************************/

struct polygon_box_rep: public box_rep {
  array<SI> x, y;
  color fill, outline;
  SI w;

  polygon_box_rep (path ip, array<SI> x, array<SI> y, SI w, color f, color o);
  operator tree () { return "polygon"; }
  void display (ps_device dev);
};

polygon_box_rep::polygon_box_rep (
  path ip, array<SI> X, array<SI> Y, SI W, color f, color o):
    box_rep (ip), x (X), y (Y), fill (f), outline (o), w (W)
{
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
polygon_box_rep::display (ps_device dev) {
  dev->set_color (fill);
  dev->polygon (x, y);
  if (w>0) {
    int i, n= N(x);
    dev->set_line_style (w);
    dev->set_color (outline);
    for (i=0; i<n; i++)
      dev->line (x[i], y[i], x[(i+1)%n], y[(i+1)%n]);
  }
}

/******************************************************************************
* Arc boxes
******************************************************************************/

struct arc_box_rep: public box_rep {
  SI    X1, Y1, X2, Y2;
  int   a1, a2;
  SI    width;
  color col;

  arc_box_rep (path ip, SI X1b, SI Y1b, SI X2b, SI Y2b,
	       int A1, int A2, SI w, color c);
  operator tree () { return "arc"; }
  void display (ps_device dev);
};

arc_box_rep::arc_box_rep (path ip, SI X1b, SI Y1b, SI X2b, SI Y2b,
			  int a1b, int a2b, SI w, color c): box_rep (ip)
{
  X1     = X1b;
  Y1     = Y1b;
  X2     = X2b;
  Y2     = Y2b;
  a1     = a1b;
  a2     = a2b;
  width  = w;
  col    = c;

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

  x1= min (p1, p2); y1= min (q1, q2);
  x2= max (p1, p2); y2= max (q1, q2);
  x3= x1-(w>>1);    y3= y1-(w>>1); 
  x4= x2+(w>>1);    y4= y2+(w>>1);
}

void
arc_box_rep::display (ps_device dev) {
  dev->set_line_style (width);
  dev->set_color (col);
  dev->arc (X1, Y1, X2, Y2, a1, a2-a1);
  // dev->line (x1, y1, x2, y2);
}

/******************************************************************************
* Image boxes
******************************************************************************/

struct image_box_rep: public box_rep {
  url u;
  double cx1, cy1, cx2, cy2;

  image_box_rep (path ip, url u2, SI w, SI h,
		 double cx1, double cy1, double cx2, double cy2);
  operator tree () { return "image"; }
  void display (ps_device dev);
};

image_box_rep::image_box_rep (
  path ip, url u2,
  SI w, SI h, double cx1b, double cy1b, double cx2b, double cy2b):
    box_rep (ip), u (u2)
{
  cx1= cx1b; cy1= cy1b;
  cx2= cx2b; cy2= cy2b;
  x1= x3= 0; y1= y3= 0;
  x2= x4= w; y2= y4= h;
}

void
image_box_rep::display (ps_device dev) {
  dev->image (u, x2, y2, 0, 0, cx1, cy1, cx2, cy2);
}

/******************************************************************************
* Control boxes
******************************************************************************/

struct control_tree_box_rep: public box_rep {
  tree t;
  control_tree_box_rep (path ip, tree t2, font fn): box_rep (ip), t (t2) {
    x1=x2=x3=x4=y1=y3=y4=0; y2=fn->yx; }
  operator tree () { return tuple ("control tree", (tree) t); }
  void display (ps_device dev) { (void) dev; }
  tree get_leaf_tree () { return t; }
};

struct control_lazy_box_rep: public box_rep {
  lazy lz;
  control_lazy_box_rep (path ip, lazy lz2, font fn): box_rep (ip), lz (lz2) {
    x1=x2=x3=x4=y1=y3=y4=0; y2=fn->yx; }
  operator tree () { return tuple ("control lazy", (tree) lz); }
  void display (ps_device dev) { (void) dev; }
  lazy get_leaf_lazy () { return lz; }
};

/******************************************************************************
* box construction routines
******************************************************************************/

box
test_box (path ip) {
  return new test_box_rep (ip);
}

box
line_box (path ip, SI x1, SI y1, SI x2, SI y2, SI w, SI c) {
  return new line_box_rep (ip, x1, y1, x2, y2, w, c);
}

box
arc_box (path ip, SI x1, SI y1, SI x2, SI y2, int a1, int a2, SI w, SI c) {
  return new arc_box_rep (ip, x1, y1, x2, y2, a1, a2, w, c);
}

box
polygon_box (path ip, array<SI> x, array<SI> y, SI w, color cf, color cl) {
  return new polygon_box_rep (ip, x, y, w, cf, cl);
}

box
polygon_box (path ip, array<SI> x, array<SI> y, color c) {
  return new polygon_box_rep (ip, x, y, 0, c, c);
}

box
image_box (path ip, url u, SI w, SI h,
	   double cx1, double cy1, double cx2, double cy2)
{
  return new image_box_rep (ip, u, w, h, cx1, cy1, cx2, cy2);
}

box
control_box (path ip, tree t, font fn) {
  return new control_tree_box_rep (ip, t, fn);
}

box
control_box (path ip, lazy lz, font fn) {
  return new control_lazy_box_rep (ip, lz, fn);
}
