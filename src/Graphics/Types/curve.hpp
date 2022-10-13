
/******************************************************************************
* MODULE     : curve.hpp
* DESCRIPTION: mathematical curves
* COPYRIGHT  : (C) 2003  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef CURVE_H
#define CURVE_H
#include "point.hpp"

class curve_rep: public abstract_struct {
public:
  inline curve_rep () {}
  inline virtual ~curve_rep () {}

  inline virtual int nr_components () { return 1; }
  // the number of components of the curve is useful for getting
  // nice parameterizations when concatenating curves

  virtual point evaluate (double t) = 0;
  // gives a point on the curve for its intrinsic parameterization
  // curves are parameterized from 0.0 to 1.0

  array<point> rectify (double eps);
  // returns a rectification of the curve, which, modulo reparameterization
  // has a uniform distance of at most 'eps' to the original curve

  virtual void rectify_cumul (array<point>& a, double eps) = 0;
  // add rectification of the curve  (except for the starting point)
  // to an existing polysegment

  /*
  NOTE: more routines should be added later so that one
  can reliably compute the intersections between curves
  One might for instance take the following:
  */
  virtual double bound (double t, double eps) = 0;
  // return delta such that |t' - t| < delta => |c(t') - c(t)| < eps.

  virtual point grad (double t, bool& error) = 0;
  // compute the first derivative at t.
  // set error= true if this derivative does not exist.

  virtual double curvature (double t1, double t2) = 0;
  // compute a bound for the second derivative between t1 and t2.
  /* FIXME: What is computed is *really* a bound for the curvature,
       not for the norm of the second derivative. Make it precise
       what it is that is computed exactly. */
  // return a very large number if such a bound does not exist.

  // returns the number of control points which belong to the curve.
  // these control points are ordered and come first in pts & cips.
  virtual int get_control_points (
            array<double>&abs, array<point>& pts, array<path>& cip);

  virtual array<double> find_closest_points (
            double t1, double t2, point p, double eps);

  virtual double find_closest_point (
            double t1, double t2, point p, double eps, bool& found);
};

class curve {
  ABSTRACT_NULL(curve);
  inline point operator () (double t) { return rep->evaluate (t); }
  inline bool operator == (curve c) { return rep == c.rep; }
  inline bool operator != (curve c) { return rep != c.rep; }
};
ABSTRACT_NULL_CODE(curve);

curve segment (point p1, point p2);
curve poly_segment (array<point> a, array<path> cip);
curve spline (
  array<point> a, array<path> cip, bool close=false, bool interpol=true);
curve bezier (array<point> a);
curve poly_bezier (array<point> a, array<path> cip, bool simple, bool closed);
curve arc (array<point> a, array<path> cip, bool close=false);
curve compound (array<curve> cs);
curve invert (curve c);
curve part (curve c, double start, double end);
curve truncate (curve c, double portion, double eps);
curve recontrol (curve c, array<point> a, array<path> cip);

array<point> intersection (curve f, curve g, point p0, double eps);
point closest (curve f, point p);

#endif // defined CURVE_H
