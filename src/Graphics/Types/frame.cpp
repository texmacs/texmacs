
/******************************************************************************
* MODULE     : frame.cpp
* DESCRIPTION: coordinate frames
* COPYRIGHT  : (C) 2003  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "frame.hpp"
#include "matrix.hpp"

/******************************************************************************
* Bounding boxes for transformed rectangles
******************************************************************************/

void
frame::enclose (double& x1, double& y1, double& x2, double& y2,
                point p1, point p2, bool direct) {
  int n= 1;
  if (!rep->linear) n= 20;
  for (int i=0; i<n; i++) {
    point p= p1 + (((double) i) / ((double) n)) * (p2 - p1);
    point q= (direct? operator () (p): operator [] (p));
    x1= min (x1, q[0]);
    y1= min (y1, q[1]);
    x2= max (x2, q[0]);
    y2= max (y2, q[1]);
  }
}

rectangle
frame::enclose (rectangle r, bool direct) {
  double x1= 1.0e100, y1= 1.0e100, x2= -1.0e100, y2= -1.0e100;
  point p1= point (r->x1, r->y1);
  point p2= point (r->x2, r->y1);
  point p3= point (r->x2, r->y2);
  point p4= point (r->x1, r->y2);
  enclose (x1, y1, x2, y2, p1, p2, direct);
  enclose (x1, y1, x2, y2, p2, p3, direct);
  enclose (x1, y1, x2, y2, p3, p4, direct);
  enclose (x1, y1, x2, y2, p4, p1, direct);
  return rectangle ((SI) floor (x1 + 0.5), (SI) floor (y1 + 0.5),
                    (SI) floor (x2 + 0.5), (SI) floor (y2 + 0.5));
}

rectangle
frame::operator () (rectangle r) {
  return enclose (r, true);
}

rectangle
frame::operator [] (rectangle r) {
  return enclose (r, false);
}

/******************************************************************************
* Shift
******************************************************************************/

struct shift_2D_rep: public frame_rep {
  point d;
  shift_2D_rep (point d2): d (d2) { linear= true; }
  operator tree () { return tuple ("shift_2D", as_tree (d)); }
  point direct_transform (point p) { return p + d; }
  point inverse_transform (point p) { return p - d; }
  point jacobian (point p, point v, bool &error) {
    (void) p; error= false; return v; }
  point jacobian_of_inverse (point p, point v, bool &error) {
    (void) p; error= false; return v; }
  double direct_bound (point p, double eps) { (void) p; return eps; }
  double inverse_bound (point p, double eps) { (void) p; return eps; }
};

frame
shift_2D (point d) {
  return tm_new<shift_2D_rep> (d);
}

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
  point jacobian_of_inverse (point p, point v, bool &error) {
    (void) p; error= false; return v / magnify; }
  double direct_bound (point p, double eps) { (void) p; return eps / magnify; }
  double inverse_bound (point p, double eps) { (void) p; return eps * magnify; }
};

frame
scaling (double magnify, point shift) {
  return tm_new<scaling_rep> (magnify, shift);
}

struct an_scaling_rep: public frame_rep {
  point magnify;
  point shift;
  an_scaling_rep (point m, point s): magnify (m), shift (s) { linear= true; }
  operator tree () {
    return tuple ("scale", as_string (magnify), as_tree (shift)); }
  point direct_transform (point p) { return shift + magnify * p; }
  point inverse_transform (point p) { return (p - shift) / magnify; }
  point jacobian (point p, point v, bool &error) {
    (void) p; error= false; return magnify * v; }
  point jacobian_of_inverse (point p, point v, bool &error) {
    (void) p; error= false; return v / magnify; }
  double direct_bound (point p, double eps) {
    (void) p; return eps / min (abs (magnify)); }
  double inverse_bound (point p, double eps) {
    (void) p; return eps * max (abs (magnify)); }
};

frame
scaling (point magnify, point shift) {
  return tm_new<an_scaling_rep> (magnify, shift);
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
  point jacobian_of_inverse (point p, point v, bool &error) {
    (void) p; error= false; return rotate_2D (v, point (0.0, 0.0), -angle); }
  double direct_bound (point p, double eps) { (void) p; return eps; }
  double inverse_bound (point p, double eps) { (void) p; return eps; }
};

frame
rotation_2D (point center, double angle) {
  return tm_new<rotation_2D_rep> (center, angle);
}

/******************************************************************************
* Slantings
******************************************************************************/

struct slanting_rep: public frame_rep {
  point center;
  double slant;
  slanting_rep (point c, double s): center (c), slant (s) { linear= true; }
  operator tree () {
    return tuple ("slanting", as_string (center), as_string (slant)); }
  point direct_transform (point p) {
    return slanted (p - center,  slant) + center; }
  point inverse_transform (point p) {
    return slanted (p - center, -slant) + center; }
  point jacobian (point p, point v, bool &error) {
    (void) p; error= false; return slanted (v, slant); }
  point jacobian_of_inverse (point p, point v, bool &error) {
    (void) p; error= false; return slanted (v, slant); }
  double direct_bound (point p, double eps) {
    (void) p; return eps / sqrt (1.0 + slant*slant); }
  double inverse_bound (point p, double eps) {
    (void) p; return eps / sqrt (1.0 + slant*slant); }
};

frame
slanting (point center, double slant) {
  return tm_new<slanting_rep> (center, slant);
}

/******************************************************************************
* Linear transformations
******************************************************************************/

struct linear_2D_rep: public frame_rep {
  matrix<double> m, u;
  linear_2D_rep (matrix<double> m2): m (m2), u (invert (m)) {
    linear= true; }
  operator tree () {
    return tuple ("linear_2D", as_tree (m)); }
  point direct_transform (point p) { return m * p; }
  point inverse_transform (point p) { return u * p; }
  point jacobian (point p, point v, bool &error) {
    (void) p; error= false; return m * v; }
  point jacobian_of_inverse (point p, point v, bool &error) {
    (void) p; error= false; return u * v; }
  double direct_bound (point p, double eps) { (void) p; return eps; }
  double inverse_bound (point p, double eps) { (void) p; return eps; }
};

frame
linear_2D (matrix<double> m) {
  return tm_new<linear_2D_rep> (m);
}

/******************************************************************************
* Affine transformations
******************************************************************************/

struct affine_2D_rep: public frame_rep {
  matrix<double> m, j;
  affine_2D_rep (matrix<double> m2): m (m2) {
    j= copy (m);
    linear= true; }
 // FIXME: Do we use "linear" in such a
 //   weakest sense for affine transforms ?
  operator tree () {
    return tuple ("affine_2D", as_tree (m)); }
  point direct_transform (point p) {
    point q= point (3), r;
    q[0]= p[0]; q[1]= p[1]; q[2]= 1.0;
    q= m * q;
    return point (q[0], q[1]); }
  point inverse_transform (point p) {
    FAILED ("not yet implemented");
    return p; }
  point jacobian (point p, point v, bool &error) {
    (void) p; error= false; return j * v; }
  point jacobian_of_inverse (point p, point v, bool &error) {
    (void) p; (void) v; (void) error;
    FAILED ("not yet implemented");
    return p;}
  double direct_bound (point p, double eps) { (void) p; return eps; }
  double inverse_bound (point p, double eps) { (void) p; return eps; }
};

frame
affine_2D (matrix<double> m) {
  return tm_new<affine_2D_rep> (m);
}

/******************************************************************************
* Vertical bending
******************************************************************************/

struct bend_frame_rep: public frame_rep {
  double (*fun) (double);
  bend_frame_rep (double (*fun2) (double)): fun (fun2) {}
  operator tree () {
    return tuple ("bend"); }
  point direct_transform (point p) {
    return point (p[0], p[1] + fun (p[0])); }
  point inverse_transform (point p) {
    return point (p[0], p[1] - fun (p[0])); }
  point jacobian (point p, point v, bool &error) {
    (void) p; (void) v; (void) error;
    FAILED ("not yet implemented");
    return p; }
  point jacobian_of_inverse (point p, point v, bool &error) {
    (void) p; (void) v; (void) error;
    FAILED ("not yet implemented");
    return p; }
  double direct_bound (point p, double eps) { (void) p; return eps; }
  double inverse_bound (point p, double eps) { (void) p; return eps; }
};

frame
bend_frame (double (*fun) (double)) {
  return tm_new<bend_frame_rep> (fun);
}

frame
bend_frame (double (*fun) (double),
            double x1, double y1, double x2, double y2) {
  frame phi= scaling (point (x2 - x1, y2 - y1), point (x1, y1));
  frame fr = bend_frame (fun);
  return phi * fr *invert (phi);
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
    point w1= f1->jacobian (f2 (p), w2, error);
    error |= error2;
    return w1;
  }
  point jacobian_of_inverse (point p, point v, bool &error) {
    (void) p; (void) v; (void) error;
    FAILED ("not yet implemented");
    return p;
  }
  double direct_bound (point p, double eps) {
    return f1->direct_bound (f2(p), f2->direct_bound (p, eps)); }
  double inverse_bound (point p, double eps) {
    return f2->inverse_bound (f1[p], f1->inverse_bound (p, eps)); }
};

frame
operator * (frame f1, frame f2) {
  return tm_new<compound_frame_rep> (f1, f2);
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
    return f->jacobian_of_inverse (p, v, error); }
  point jacobian_of_inverse (point p, point v, bool &error) {
    return f->jacobian (p, v, error); }
  double direct_bound (point p, double eps) {
    return f->inverse_bound (p, eps); }
  double inverse_bound (point p, double eps) {
    return f->direct_bound (p, eps); }
};

frame
invert (frame f) {
  return tm_new<inverted_frame_rep> (f);
}
