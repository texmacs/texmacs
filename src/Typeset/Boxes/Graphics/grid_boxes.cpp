
/******************************************************************************
* MODULE     : grid_boxes.cpp
* DESCRIPTION: grid boxes for the graphics
* COPYRIGHT  : (C) 2003  Henri Lesourd
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "Graphics/grid.hpp"
#include "Graphics/point.hpp"
#include "Graphics/frame.hpp"
#include "Boxes/graphics.hpp"
#include "Boxes/composite.hpp"
#include <math.h>

/******************************************************************************
* Grid boxes
******************************************************************************/

struct grid_box_rep: public box_rep {
  grid g;
  frame f;
  bool first_time;
  int dev_pixel;
  array<box> bs;
  grid_box_rep (
    path ip, grid g, frame f, point lim1, point lim2);
  void display (ps_device dev);
  operator tree () { return (tree)g; }
  path find_lip () { return path (-1); }
  path find_rip () { return path (-1); }
  gr_selections graphical_select (SI x, SI y, SI dist);
  int reindex (int i, int item, int n);
};

static bool
array_point_closest (array<point> a1, array<point> a2, double dist) {
  int i, n= N(a1);
  if (n != N(a2)) return false;
  for (i=0; i<n; i++) {
    if (norm (a1[i] - a2[i]) >= dist)
      return false;
  }
  return true;
}

grid_box_rep::grid_box_rep (
  path ip2, grid g2, frame f2, point lim1, point lim2):
    box_rep (ip2), g(g2), f(f2)
{
  first_time= true;
  point flim1= f(lim1), flim2= f(lim2);
  x1= x3= (SI) min (flim1[0], flim2[0]);
  y1= y3= (SI) min (flim1[1], flim2[1]);
  x2= x4= (SI) max (flim1[0], flim2[0]);
  y2= y4= (SI) max (flim1[1], flim2[1]);
}

void
grid_box_rep::display (ps_device dev) {
  int i;
  if (first_time || dev->pixel!=dev_pixel) {
    point p1= f [point (x1, y1)];
    point p2= f [point (x2, y2)];
    point l1= point (min (p1[0], p2[0]), min (p1[1], p2[1]));
    point l2= point (max (p1[0], p2[0]), max (p1[1], p2[1]));
    array<grid_curve> grads= g->get_curves (l1, l2);

    array<point> a= array<point> (0);
    array<point> aold= array<point> (0);
    for (i=0; i<N(grads); i++) {
      curve c= f (grads[i].c);
      array<point> a= c->rectify (dev->pixel);
      if (!array_point_closest (aold, a, 2*dev->pixel)) {
	aold= a;
	bs << curve_box (
		decorate (ip), c, dev->pixel, dev->get_color (grads[i].col));
      }
    }
    first_time= false;
    dev_pixel= dev->pixel;
  }
  for (i=0; i<N(bs); i++)
    bs[i]->display (dev);
}

gr_selections
grid_box_rep::graphical_select (SI x, SI y, SI dist) {
  gr_selections res;
  return res;
}

int
grid_box_rep::reindex (int i, int item, int n) {
  return i;
}

/******************************************************************************
* User interface
******************************************************************************/

box
grid_box (path ip, grid g, frame f, point lim1, point lim2) {
  return new grid_box_rep (ip, g, f, lim1, lim2);
}
