
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

class frame_rep: public abstract_struct {
public:
  bool linear;
public:
  inline frame_rep () { linear= false; }
  inline virtual ~frame_rep () {}

  virtual point direct_transform (point p) = 0;
  virtual point inverse_transform (point p) = 0;
  // a point p in frame f corresponds to f(p) = f->direct_transform(p)
  // in the parent frame of f

  virtual double direct_bound (point p, double err) = 0;
  virtual double inverse_bound (point p, double err) = 0;
  // direct_bound (p, epsilon) yields a delta such that for all p'
  // we have |p' - p| < delta => |f(p') - f(p)| < epsilon.
};

class curve;
class frame {
  ABSTRACT_NULL(frame);
  inline point operator () (point p) { return rep->direct_transform (p); }
  inline point operator [] (point p) { return rep->inverse_transform (p); }
  inline bool operator == (frame f) { return rep == f.rep; }
  inline bool operator != (frame f) { return rep != f.rep; }
  curve operator () (curve c);
  curve operator [] (curve c);
};
ABSTRACT_NULL_CODE(frame);

frame scaling (double magnify, array<double> shift);
frame operator * (frame f1, frame f2);
frame invert (frame f);

#endif // defined FRAME_H
