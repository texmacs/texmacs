
/******************************************************************************
* MODULE     : graphics.cpp
* DESCRIPTION: Boxes for graphics
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "Boxes/graphics.hpp"
#include "Boxes/composite.hpp"
#include <math.h>

/******************************************************************************
* Graphics boxes
******************************************************************************/

struct graphics_box_rep: public composite_box_rep {
  frame f;
  point lim1, lim2;
  graphics_box_rep (path ip, array<box> bs, frame f, point lim1, point lim2);
  frame get_frame ();
  void  get_limits (point& lim1, point& lim2);
  operator tree () { return "graphics"; }
};

graphics_box_rep::graphics_box_rep (
  path ip2, array<box> bs2, frame f2, point lim1b, point lim2b):
  composite_box_rep (ip2, bs2), f (f2), lim1 (lim1b), lim2 (lim2b)
{
  point flim1= f(lim1), flim2= f(lim2);
  x1= (SI) min (flim1[0], flim2[0]);
  y1= (SI) min (flim1[1], flim2[1]);
  x2= (SI) max (flim1[0], flim2[0]);
  y2= (SI) max (flim1[1], flim2[1]);
  finalize ();
}

frame
graphics_box_rep::get_frame () {
  return f;
}

void
graphics_box_rep::get_limits (point& lim1b, point& lim2b) {
  lim1b= lim1;
  lim2b= lim2;
}

/******************************************************************************
* Point boxes
******************************************************************************/

struct point_box_rep: public box_rep {
  point p;
  SI r;
  color col;
  string style;
  point_box_rep (path ip, point p, SI radius, color col, string style);
  SI graphical_distance (SI x, SI y) { return (SI)norm (p - point (x, y)); }
  void display (ps_device dev);
  operator tree () { return "point"; }
};

point_box_rep::point_box_rep (
  path ip2, point p2, SI r2, color col2, string style2):
    box_rep (ip2), p (p2), r (r2), col (col2), style (style2)
{
  x1= x3= ((SI) p[0]) - r;
  y1= y3= ((SI) p[1]) - r;
  x2= x4= ((SI) p[0]) + r;
  y2= y4= ((SI) p[1]) + r;
}

void
point_box_rep::display (ps_device dev) {
  if (style == "square") {
    dev->set_color (col);
    dev->set_line_style (PIXEL);
    dev->line (((SI) p[0]) - r, ((SI) p[1]) - r,
	       ((SI) p[0]) - r, ((SI) p[1]) + r); 
    dev->line (((SI) p[0]) + r, ((SI) p[1]) + r,
	       ((SI) p[0]) - r, ((SI) p[1]) + r); 
    dev->line (((SI) p[0]) + r, ((SI) p[1]) + r,
	       ((SI) p[0]) + r, ((SI) p[1]) - r); 
    dev->line (((SI) p[0]) - r, ((SI) p[1]) - r,
	       ((SI) p[0]) + r, ((SI) p[1]) - r); 
  }
  else {
    int i, n= 4*(r/dev->pixel+1);
    array<SI> x (n), y (n);
    for (i=0; i<n; i++) {
      x[i]= (SI) (p[0] + r * cos ((6.283185307*i)/n));
      y[i]= (SI) (p[1] + r * sin ((6.283185307*i)/n));
    }
    dev->set_color (col);
    dev->polygon (x, y);
  }
}

/******************************************************************************
* Curve boxes
******************************************************************************/

struct curve_box_rep: public box_rep {
  array<point> a;
  SI width;
  color col;
  curve c;
  curve_box_rep (path ip, curve c, SI width, color col);
  SI graphical_distance (SI x, SI y);
  gr_selections graphical_select (SI x, SI y, SI dist);
  void display (ps_device dev);
  operator tree () { return "curve"; }
};

curve_box_rep::curve_box_rep (path ip2, curve c2, SI W, color C):
  box_rep (ip2), width (W), col (C), c (c2)
{
  a= c->rectify (PIXEL);
  int i, n= N(a);
  x1= y1= x3= y3= MAX_SI;
  x2= y2= x4= y4= -MAX_SI;
  for (i=0; i<(n-1); i++) {
    x1= min (x1, min ((SI) a[i][0], (SI) a[i+1][0]));
    y1= min (y1, min ((SI) a[i][1], (SI) a[i+1][1]));
    x2= max (x2, max ((SI) a[i][0], (SI) a[i+1][0]));
    y2= max (y2, max ((SI) a[i][1], (SI) a[i+1][1]));
  }
  x3= x1 - (width>>1); y3= y1 - (width>>1); 
  x4= x2 + (width>>1); y4= y2 + (width>>1);
}

SI
curve_box_rep::graphical_distance (SI x, SI y) {
  SI gd= MAX_SI;
  point p (x, y);
  int i;
  for (i=0; i<N(a)-1; i++) {
    axis ax;
    ax.p0= a[i];
    ax.p1= a[i+1];
    gd= min (gd, (SI)seg_dist (ax, p));
  }
  return gd;
}

gr_selections
curve_box_rep::graphical_select (SI x, SI y, SI dist) {
  gr_selections res;
  if (graphical_distance (x, y) <= dist) {
    array<double> abs;
    array<point> pts;
    array<path> paths;
    int np= c->get_control_points (abs, pts, paths);
    point p (x, y);
    int i;
    for (i=0; i<N(pts); i++) {
      SI n= (SI)norm (p - pts[i]);
      if (n <= dist) {
        gr_selection gs;
        gs->dist= n;
        gs->cp << reverse (paths[i]);
        res << gs;
      }
    }
    int ne= np-1;
    if (np>1 && (abs[0]!=0.0 || abs[np-1]!=1.0))
      ne++;
    for (i=0; i<ne; i++) {
      bool b;
      double t= c->find_closest_point (abs[i], abs[(i+1)%np], p, PIXEL, b);
      if (b) {
        point p2= c->evaluate (t);
        SI n= (SI)norm (p - p2);
        if (n <= dist) {
          gr_selection gs;
          gs->dist= n;
          gs->cp << reverse (paths[i]);
          gs->cp << reverse (paths[(i+1)%np]);
          res << gs;
        }
      }
    }
  }
  return res;
}

void
curve_box_rep::display (ps_device dev) {
  int i, n= N(a);  
  dev->set_line_style (width);
  dev->set_color (col);
  for (i=0; i<(n-1); i++)
    dev->line ((SI) a[i][0], (SI) a[i][1], (SI) a[i+1][0], (SI) a[i+1][1]);
}

/******************************************************************************
* User interface
******************************************************************************/

box
graphics_box (path ip, array<box> bs, frame f, point lim1, point lim2) {
  return new graphics_box_rep (ip, bs, f, lim1, lim2);
}

box
point_box (path ip, point p, SI r, color col, string style) {
  return new point_box_rep (ip, p, r, col, style);
}

box
curve_box (path ip, curve c, SI width, color col) {
  return new curve_box_rep (ip, c, width, col);
}
