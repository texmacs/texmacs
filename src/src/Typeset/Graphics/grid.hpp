
/******************************************************************************
* MODULE     : grid.hpp
* DESCRIPTION: grids for the graphics
* COPYRIGHT  : (C) 2003  Henri Lesourd
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef GRID_H
#define GRID_H
#include "point.hpp"
#include "frame.hpp"
#include "ps_device.hpp"

class grid_rep: public abstract_struct {
protected:
  array<SI> subd;
  array<color> col;
  point o; // Center
public:
  inline grid_rep () {}
  inline grid_rep (array<SI> subd2, array<color> col2, point o2):
    subd(subd2), col(col2), o(o2) {}
  inline virtual ~grid_rep () {}

  virtual operator tree () = 0;
  virtual void display (ps_device dev, SI x1, SI y1, SI x2, SI y2) = 0;
  virtual point find_closest_point (point p) = 0;
};

class grid {
  ABSTRACT_NULL(grid);
  operator tree () { return (tree)*rep; }
};
ABSTRACT_NULL_CODE(grid);

grid empty_grid();
grid cartesian (array<SI> subd, array<color> col, point o, SI step);
grid polar (array<SI> subd, array<color> col, point o, SI step, SI astep);
grid logarithmic (array<SI> subd, array<color> col, point o, SI step, SI base);

#endif // defined GRID_H
