
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
  double magnify;
  point  shift;
  scaling_rep (double m, point s): magnify (m), shift (s) { linear= true; }
  operator tree () {
    return tuple ("scale", as_string (magnify), as_tree (shift)); }
  point direct_transform (point p) { return shift + magnify * p; }
  point inverse_transform (point p) { return (p - shift) / magnify; }
  point jacobian (point p, point v, bool &error) {
    (void) p; error= false; return magnify * v; }
  double direct_bound (point p, double eps) { return eps / magnify; }
  double inverse_bound (point p, double eps) { return eps * magnify; }
};

frame
scaling (double magnify, point shift) {
  return new scaling_rep (magnify, shift);
}

/******************************************************************************
* Rotations
******************************************************************************/

struct rotation_2D_rep: public frame_rep {
  point center;
  double angle;
  rotation_2D_rep (point o, double a): center (o), angle (a) { linear= true; }
  operator tree () {
    return tuple ("rotation_2D", as_tree (center), as_string (angle)); }
  point direct_transform (point p) { return rotate_2D (p, center, angle); }
  point inverse_transform (point p) { return rotate_2D (p, center, -angle); }
  point jacobian (point p, point v, bool &error) {
    (void) p; error= false; return rotate_2D (v, point (0.0, 0.0), angle); }
  double direct_bound (point p, double eps) { return eps; }
  double inverse_bound (point p, double eps) { return eps; }
};

frame
rotation_2D (point center, double angle) {
  return new rotation_2D_rep (center, angle);
}

/******************************************************************************
* Compound frames
******************************************************************************/

struct compound_frame_rep: public frame_rep {
  frame f1, f2;
  compound_frame_rep (frame f1b, frame f2b):
    f1 (f1b), f2 (f2b) { linear= f1->linear && f2->linear; }
  operator tree () { return tuple ("compound", (tree) f1, (tree) f2); }
  point direct_transform (point p) { return f1 (f2 (p)); }
  point inverse_transform (point p) { return f2 [f1 [p]]; }
  point jacobian (point p, point v, bool &error) {
    bool error2;
    point w2= f2->jacobian (p, v, error2);
    point w1= f1->jacobian (f2 (p), w1, error);
    error |= error2;
    return w1;
  }
  double direct_bound (point p, double eps) {
    return f1->direct_bound (f2(p), f2->direct_bound (p, eps)); }
  double inverse_bound (point p, double eps) {
    return f2->inverse_bound (f1[p], f1->inverse_bound (p, eps)); }
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
  operator tree () { return tuple ("inverse", (tree) f); }
  point direct_transform (point p) { return f [p]; }
  point inverse_transform (point p) { return f (p); }
  point jacobian (point p, point v, bool &error) {
    fatal_error ("Not yet implemented",
		 "inverted_frame_rep::jacobian");
  }
  double direct_bound (point p, double eps) {
    return f->inverse_bound (p, eps); }
  double inverse_bound (point p, double eps) {
    return f->direct_bound (p, eps); }
};

frame
invert (frame f) {
  return new inverted_frame_rep (f);
}
