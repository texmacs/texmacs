
/******************************************************************************
* MODULE     : frame.hpp
* DESCRIPTION: coordinate frames
* COPYRIGHT  : (C) 2003  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#ifndef FRAME_H
#define FRAME_H
#include "point.hpp"
#include "matrix.hpp"

class frame_rep: public abstract_struct {
public:
  bool linear;
public:
  inline frame_rep () { linear= false; }
  inline virtual ~frame_rep () {}
  virtual operator tree () = 0;

  virtual point direct_transform (point p) = 0;
  virtual point inverse_transform (point p) = 0;
  // a point p in frame f corresponds to f(p) = f->direct_transform(p)
  // in the parent frame of f

  virtual point jacobian (point p, point v, bool &error) = 0;
  virtual point jacobian_of_inverse (point p, point v, bool &error) = 0;
  // the Jacobian matrix at p applied to v

  virtual double direct_bound (point p, double eps) = 0;
  virtual double inverse_bound (point p, double eps) = 0;
  // direct_bound (p, eps) yields a delta such that for all p'
  // we have |p' - p| < delta => |f(p') - f(p)| < eps.

  inline double direct_scalar (double x) {
    return norm (direct_transform (point (x, 0))); }
  inline double inverse_scalar (double x) {
    return norm (inverse_transform (point (x, 0))); }
  // FIXME: used for grids, but error-prone. What is the idea?
};

class curve;
class frame {
  ABSTRACT_NULL(frame);
  operator tree () { return (tree) *rep; }
  inline point operator () (point p) { return rep->direct_transform (p); }
  inline point operator [] (point p) { return rep->inverse_transform (p); }
  inline bool operator == (frame f) { return rep == f.rep; }
  inline bool operator != (frame f) { return rep != f.rep; }
  curve operator () (curve c);
  curve operator [] (curve c);
};
ABSTRACT_NULL_CODE(frame);

// Transformations
frame scaling (double magnify, point shift);
frame rotation_2D (point center, double angle);
frame affine_2D (matrix m);

// Operations on transformations
frame operator * (frame f1, frame f2);
frame invert (frame f);

#endif // defined FRAME_H
