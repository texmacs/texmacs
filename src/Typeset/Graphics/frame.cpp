
/******************************************************************************
* MODULE     : frame.cpp
* DESCRIPTION: coordinate frames
* COPYRIGHT  : (C) 2003  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "frame.hpp"

/******************************************************************************
* Scalings
******************************************************************************/

struct scaling_rep: public frame_rep {
  double        magnify;
  array<double> shift;
  scaling_rep (double m, array<double> s): magnify (m), shift (s) {
    linear= true; }
  point direct_transform (point p) { return shift + magnify * p; }
  point inverse_transform (point p) { return (p - shift) / magnify; }
  double direct_bound (point p, double err) { return err / magnify; }
  double inverse_bound (point p, double err) { return err * magnify; }
};

frame
scaling (double magnify, array<double> shift) {
  return new scaling_rep (magnify, shift);
}

/******************************************************************************
* Compound frames
******************************************************************************/

struct compound_frame_rep: public frame_rep {
  frame f1, f2;
  compound_frame_rep (frame f1b, frame f2b):
    f1 (f1b), f2 (f2b) { linear= f1->linear && f2->linear; }
  point direct_transform (point p) { return f1 (f2 (p)); }
  point inverse_transform (point p) { return f2 [f1 [p]]; }
  double direct_bound (point p, double err) {
    return f1->direct_bound (f2(p), f2->direct_bound (p, err)); }
  double inverse_bound (point p, double err) {
    return f2->inverse_bound (f1[p], f1->inverse_bound (p, err)); }
};

frame
operator * (frame f1, frame f2) {
  return new compound_frame_rep (f1, f2);
}

/******************************************************************************
* Inverted frames
******************************************************************************/

struct inverted_frame_rep: public frame_rep {
  frame f;
  inverted_frame_rep (frame f2): f (f2) { linear= f->linear; }
  point direct_transform (point p) { return f [p]; }
  point inverse_transform (point p) { return f (p); }
  double direct_bound (point p, double err) {
    return f->inverse_bound (p, err); }
  double inverse_bound (point p, double err) {
    return f->direct_bound (p, err); }
};

frame
invert (frame f) {
  return new inverted_frame_rep (f);
}
