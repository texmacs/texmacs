
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
  grid_box_rep (path ip, grid g, frame f, point lim1, point lim2);
  operator tree () { return (tree)g; }
  void display (ps_device dev);
};

grid_box_rep::grid_box_rep (
  path ip2, grid g2, frame f, point lim1, point lim2):
  box_rep (ip2), g(g2)
{
  point flim1= f(lim1), flim2= f(lim2);
  x1= x3= (SI) min (flim1[0], flim2[0]);
  y1= y3= (SI) min (flim1[1], flim2[1]);
  x2= x4= (SI) max (flim1[0], flim2[0]);
  y2= y4= (SI) max (flim1[1], flim2[1]);
}

void
grid_box_rep::display (ps_device dev) {
  g->display (dev, x1, y1, x2, y2);
}

/******************************************************************************
* User interface
******************************************************************************/

box
grid_box (path ip, grid g, frame f, point lim1, point lim2) {
  return new grid_box_rep (ip, g, f, lim1, lim2);
}
