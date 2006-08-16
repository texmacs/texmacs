
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
#include "frame.hpp"
#include "curve.hpp"
#include "ps_device.hpp"

class grid_curve_rep: public concrete_struct {
public:
  string col;
  curve c;
  inline grid_curve_rep () {}
  inline grid_curve_rep (string col2, curve c2):
    col(col2), c(c2) {}
};

class grid_curve {
  CONCRETE_NULL(grid_curve);
  inline grid_curve (string col, curve c):
    rep (new grid_curve_rep (col, c)) {}
};
CONCRETE_NULL_CODE(grid_curve);

class grid_rep: public abstract_struct {
protected:
  array<SI> subd;
  array<string> col;
  point center;
public:
  inline grid_rep () {}
  inline grid_rep (array<SI> subd2, array<string> col2, point o):
    subd(subd2), col(col2), center(o) {}
  inline virtual ~grid_rep () {}

  virtual void set_aspect (tree aspect);

  virtual operator tree () = 0;
  virtual array<grid_curve> get_curves (
    point lim1, point lim2, double u, bool b= false) = 0;
  virtual point find_closest_point (point p, point pmin, point pmax) = 0;
};

class grid {
  ABSTRACT_NULL(grid);
  operator tree () { return (tree)*rep; }
};
ABSTRACT_NULL_CODE(grid);

grid empty_grid();
grid cartesian (array<SI> subd, array<string> col, point o, double step);
grid polar (array<SI> subd, array<string> col, point o, double step, SI astep);
grid logarithmic (array<SI> subd, array<string> col, point o,
		  double step, SI base);

grid as_grid (tree t);
tree as_tree (grid g);

#endif // defined GRID_H
