
/******************************************************************************
* MODULE     : curve.cpp
* DESCRIPTION: mathematical curves
* COPYRIGHT  : (C) 2003  Joris van der Hoeven and Henri Lesourd
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "path.hpp"
#include "curve.hpp"
#include "frame.hpp"
#include "equations.hpp"
#include "math_util.hpp"
#include "polynomial.hpp"
#include "merge_sort.hpp"

/******************************************************************************
* General routines
******************************************************************************/

array<point>
curve_rep::rectify (double eps) {
  array<point> a (1);
  a[0]= evaluate (0.0);
  rectify_cumul (a, eps);
  return a;
}

double
curve_rep::bound (double t, double eps) {
  //TODO: Improve this, as soon as the curvature ()
  //      for transformed_curves will be implemented
  bool b;
  double ng= norm (grad (t, b));
  if (ng <= 1.0e-12)
    return tm_infinity;
  double delta= eps / ng;
  while (delta >= 1.0e-6) {
    point val1 = evaluate (t);
    point val2 = evaluate (max (t-delta, 0.0));
    point val3 = evaluate (min (t+delta, 1.0));
    if (norm (val2 - val1) <= eps+1.0e-6 &&
	norm (val3 - val1) <= eps+1.0e-6)
      return delta;
    delta /= 2.0;
  }
  return delta;
}

int
curve_rep::get_control_points (
  array<double>&abs, array<point>& pts, array<path>& cip)
{
  abs= array<double>();
  pts= array<point>();
  cip= array<path>();
  return 0;
}

struct curvet {
  double t, dist;
};

struct less_eq_curvet {
  static inline bool leq (curvet& a, curvet &b) {
    return a.dist <= b.dist; }
};

static array<curvet>
curvet_closest_points (
  curve c, double t1, double t2, point p, double eps)
{
  array<curvet> res;
  if (t1>t2) {
    array<curvet> r1, r2;
    r1= curvet_closest_points (c, 0.0, t2, p, eps);
    r2= curvet_closest_points (c, t1, 1.0, p, eps);
    res= r1 << r2;
  }
  else {
    double t;
    double closest= -1;
    point pclosest;
    double n0= tm_infinity;
    bool stored= true;
    double nprec= n0;
    bool decreasing= false;
    double max_step= 0.5 / max (c->nr_components (), 1);
    for (t=t1; t<=t2;) {
      point pt= c->evaluate(t);
      double n= norm (pt - p);
      if (n < n0) {
	n0= n;
	closest= t;
	pclosest= pt;
	stored= false;
      }
      decreasing= n<nprec;
      if (!stored && !decreasing) {
        curvet ct;
        ct.dist= norm (pclosest - p);
        ct.t= closest;
	res << ct;
	stored= true;
      }
      if (stored && decreasing)
        n0= tm_infinity;
      double delta= (n - eps) / 2;
      t += min (max_step, max (0.00001, c->bound (t, max (eps, delta))));
      nprec= n;
    }
    if (!stored && decreasing) {
      curvet ct;
      ct.dist= norm (pclosest - p);
      ct.t= closest;
      res << ct;
    }
  }
  return res;
}

array<double>
curve_rep::find_closest_points (
  double t1, double t2, point p, double eps)
{
  array<curvet> res= curvet_closest_points (curve (this), t1, t2, p, eps);
  merge_sort_leq <curvet, less_eq_curvet> (res);
  array<double> rest= array<double> (N(res));
  int i;
  for (i=0; i<N(res); i++)
    rest[i]= res[i].t;
  return rest;
}

double
curve_rep::find_closest_point (
  double t1, double t2, point p, double eps, bool& found)
{
  array<double> res= find_closest_points (t1, t2, p, eps);
  found= N(res)>0;
  if (found)
    return res[0];
  else
    return -1;
}

point
closest (curve f, point p) {
  array<double> abs;
  array<point> pts;
  array<path> rcip;
  f->get_control_points (abs, pts, rcip);
  if (N(abs) == 0) return f(0);
  double t1= abs[0];
  double t2= abs[N(abs)-1];
  double best= 0;
  double eps = norm (f(0) - p);
  for (int i=0; i<10; i++) {
    bool found= false;
    double t= f->find_closest_point (t1, t2, p, eps, found);
    if (found) best= t;
    else break;
    double eps2= norm (f(t) - p);
    if (eps2 >= 0.9 * eps) break;
    eps= eps2;
  }
  return f(best);
}

point
grad (curve f, double t) {
  bool error= false;
  return f->grad (t, error);
}

bool
intersection (curve f, curve g, double& t, double& u) {
  // for two dimensional curves only
  double d= norm (f (t) - g (u));
  while (!fnull (d, 1.0e-9)) {
    point  ft = f (t);
    point  gu = g (u);
    point  gf = grad (f, t);
    point  gg = grad (g, u);
    double det= gf[0] * gg[1] - gf[1] * gg[0];
    if (fnull (det, 1.0e-12)) break;
    double dt = ((gu[0] - ft[0]) * gg[1] - (gu[1] - ft[1]) * gg[0]) / det;
    double du = ((gu[0] - ft[0]) * gf[1] - (gu[1] - ft[1]) * gf[0]) / det;
    double T  = t + dt;
    double U  = u + du;
    if (T < 0.0 || T > 1.0 || U < 0.0 || U > 1.0) break;
    double D  = norm (f (T) - g (U));
    if (D > 0.9 * d) break;
    t= T; u= U; d= D;
  }
  return fnull (d, 1.0e-9);
}

array<point>
intersection (curve f, curve g, point p0, double eps) {
  // For local intersections only
  array<point> res;
  if (is_nil (f) || is_nil (g)) return res;
  bool found= false;
  double d1, d2;
  if (f==g) {
    array<double> pts= f->find_closest_points (0.0, 1.0, p0, eps);
    if (N(pts)>=2) {
      d1= pts[0];
      d2= pts[1];
      found= intersection (f, f, d1, d2);
    }
  }
  else {
    bool res1, res2;
    d1= f->find_closest_point (0.0, 1.0, p0, eps, res1);
    d2= g->find_closest_point (0.0, 1.0, p0, eps, res2);
    if (res1 && res2)
      found= intersection (f, g, d1, d2);
  }
  if (found)
    res << f (d1);
  return res;
}

/******************************************************************************
* Segments
******************************************************************************/

struct segment_rep: public curve_rep {
  point p1, p2;
  path cip1, cip2;
  segment_rep (point p1b, point p2b): p1 (p1b), p2 (p2b) {}
  point evaluate (double t) { return (1.0-t)*p1 + t*p2; }
  void rectify_cumul (array<point>& a, double eps) { (void) eps; a << p2; }
  double bound (double t, double eps) {
    return curve_rep::bound (t, eps);
  }
  point grad (double t, bool& error) {
    (void) t;
    error= false;
    return p2 - p1;
  }
  double curvature (double t1, double t2) {
    (void) t1; (void) t2;
    return tm_infinity;
  }
  int get_control_points (
    array<double>&abs, array<point>& pts, array<path>& cip);
};

int
segment_rep::get_control_points (
  array<double>&abs, array<point>& pts, array<path>& cip)
{
  array<double> u;
  u << 0.0;
  u << 1.0;
  abs= u;
  array<point> a;
  a << p1;
  a << p2;
  pts= a;
  cip= array<path> ();
  cip << cip1;
  cip << cip2;
  return 2;
}

curve
segment (point p1, point p2) {
  return tm_new<segment_rep> (p1, p2);
}

/******************************************************************************
* Poly-segments
******************************************************************************/

struct poly_segment_rep: public curve_rep {
  array<point> a;
  array<path> cip;
  int n;
  poly_segment_rep (array<point> a2, array<path> cip2):
    a (a2), cip (cip2), n(N(a)-1) {}
  int nr_components () { return n; }
  point evaluate (double t) {
    int i= max (min ((int) (n*t), n-1), 0);
    return (i+1 - n*t)*a[i] + (n*t - i)*a[i+1];
  }
  void rectify_cumul (array<point>& cum, double eps) {
    (void) eps;
    for (int i=0; i<n; i++)
      cum << a[i+1];
  }
  double bound (double t, double eps) {
    return curve_rep::bound (t, eps);
  }
  double curvature (double t1, double t2) {
    (void) t1; (void) t2;
    return tm_infinity;
  }
  point grad (double t, bool& error) {
    error= false;
    int i= min ((int) (n*t), n-1);
    return n * (a[i+1] - a[i]);
  }
  int get_control_points (
    array<double>&abs, array<point>& pts, array<path>& cip);
};

int
poly_segment_rep::get_control_points (
  array<double>&abs, array<point>& pts, array<path>& rcip)
{
  array<double> u(n+1);
  int i;
  for (i=0; i<n+1; i++)
    u[i]= ((double)i) / (n==0?1:n);
  abs = u;
  pts = a;
  rcip= cip;
  return n+1;
}

curve
poly_segment (array<point> a, array<path> cip) {
  return tm_new<poly_segment_rep> (a, cip);
}

/******************************************************************************
* Poly-smoothed segments
******************************************************************************/

struct poly_smoothed_rep: public curve_rep {
  array<point> a;
  array<double> ts;
  array<path> cip;
  double wt;
  int n;
  poly_smoothed_rep (array<point> a2, array<double> ts2,
                     array<path> cip2, double wt2):
    a (a2), ts (ts2), cip (cip2), wt (wt2), n(N(a)-1) {}
  int nr_components () { return n; }
  int find_index (double t) {
    int lo= 0, hi= n;
    if (t <= ts[lo]) return lo;
    if (t >= ts[hi]) return hi - 1;
    while (hi - lo > 1) {
      int mid= (lo + hi) - 1;
      if (t < ts[mid]) hi= mid;
      else lo= mid;
    }
    return lo;
  }
  point old_evaluate (double t) {
    if (t <= ts[0]) return a[0];
    if (t >= ts[n]) return a[n];
    int i0= find_index (t);
    double t01= ts[i0];
    double t02= ts[i0+1];
    return ((t02-t) * a[i0] + (t-t01) * a[i0+1]) / (t02 - t01);
  }
  point evaluate (double t) { 
    double w= min (min (wt, t), 1.0 - t);
    int prec= 5;
    double weight= 0.0;
    double alpha= 3.0 / (prec * prec);
    point cum (0.0, 0.0);
    for (int i=-prec; i<=prec; i++) {
      double c= exp (-alpha * i * i);
      cum = cum + c * old_evaluate (t + (i*w) / prec);
      weight += c;
    }
    return cum / weight;
  }
  void rectify_cumul (array<point>& cum, double eps) {
    (void) eps;
    for (int i=0; i<n; i++)
      for (int j=1; j<=5; j++) {
        double t= ts[i] + (j/5.0) * (ts[i+1] - ts[i]);
        cum << evaluate (t);
      }
  }
  double bound (double t, double eps) {
    return curve_rep::bound (t, eps);
  }
  double curvature (double t1, double t2) {
    (void) t1; (void) t2;
    return tm_infinity;
  }
  point grad (double t, bool& error) {
    error= false;
    double eps= 1.0e-7;
    point v1= evaluate (t);
    point v2= evaluate (t + eps);
    return (v2 - v1) / eps;
  }
  int get_control_points (
    array<double>&abs, array<point>& pts, array<path>& rcip) {
      abs= ts; pts= a; rcip= cip; return n+1; }
};

curve
poly_smoothed (array<point> a, array<double> ts, array<path> cip, double wt) {
  int n= N(a);
  ASSERT (N(ts) == n && N(cip) == n, "incompatible lengths");
  ASSERT (n >= 2 && ts[0] == 0.0 && ts[n-1] == 1.0, "invalid end points");
  for (int i=1; i<n; i++)
    ASSERT (ts[i] > ts[i-1], "consecutive times expected");
  return tm_new<poly_smoothed_rep> (a, ts, cip, wt);
}

curve
poly_smoothed (array<point> a, array<path> cip, double strength) {
  ASSERT (N(a) >= 2, "invalid length");
  array<double> ts;
  int n= N(a) - 1;
  for (int i=0; i<=n; i++) ts << ((double) i) / n;
  return poly_smoothed (a, ts, cip, strength / N(a));
}

/******************************************************************************
* Splines
******************************************************************************/

static const double epsilon=0.01;//0.00005;
typedef polynomial<double> dpol;
typedef vector<polynomial<double> > dpols;

struct spline_rep: public curve_rep {
  array<point> a;
  array<path> cip;
  int n;
  array<double> U;
  array<dpols> p;
  bool close, interpol;

  spline_rep (
    array<point> a, array<path> cip, bool close=false, bool interpol=true);

  inline double d (int i,int k) { return U[i]-U[i-k]; }
  inline double m (int i) { return (U[i]+U[i+1])/2; }

  double convert (double u) { return U[2]+u*(U[n+1]-U[2]); }
  double unconvert (double u) { return (u-U[2])/(U[n+1]-U[2]); }
  int interval_no (double u);

  point spline (int i,double u,int o=0);
  inline double S (
    array<dpol> p1, array<dpol> p2, array<dpol> p3,
    int i, double u);
  point evaluate (double t,int o);
  point evaluate (double t);
  double bound (double t, double eps);
  point grad (double t, bool& error);

  double curvature (int i, double t1, double t2);
  double curvature (double t1, double t2);

  bool approx (int i, double u1, double u2, double eps);
  void rectify_cumul (array<point>& cum, int i,
		      double u1, double u2, double eps);
  void rectify_cumul (array<point>& cum, double eps);
  int get_control_points (
    array<double>&abs, array<point>& pts, array<path>& cip);
};

// Creation
spline_rep::spline_rep (
  array<point> a2, array<path> cip2, bool close2,bool interpol2):
  a(a2), cip(cip2), n(N(a)-1), close(close2), interpol(interpol2)
{
  array<dpol> p1,p2,p3;
  p1= array<dpol> (n+3);
  p2= array<dpol> (n+3);
  p3= array<dpol> (n+3);
  p = array<dpols> (n+3);
  U = array<double> (n+6);
  if (close) {
    int i;
    a->resize(N(a)+2);
    for (i=0;i<=1;i++)
      a[n+1+i]=a[i];
    n+=2;
  }
  int i;
  double x=0;
  if (!close) {
    for (i=0;i<3;i++) {
      U[i]=x;
      x+=epsilon; 
    }
    x+=1-epsilon;
    for (i=3;i<=n;i++) {
      U[i]=x;
      x+=1;
    }
    for (i=n+1;i<=n+3;i++) {
      U[i]=x;
      x+=epsilon;
    }
  }
  else {
    for (i=0;i<=n+3;i++) {
      U[i]=x;
      x+=1;
    }
  }
  for (i=0;i<=n;i++) {
    double di22= d(i+2,2);
    double di11= d(i+1,1);
    double di21= d(i+2,1);
    double di32= d(i+3,2);
    double di31= d(i+3,1);
    p1[i]= dpol (square(U[i])/di22/di11,
		 -2*U[i]/di22/di11,
		 1/di22/di11);
    p2[i]= dpol (-U[i+2]*U[i]/di22/di21-U[i+3]*U[i+1]/di32/di21,
		 (U[i+2]+U[i])/di22/di21+(U[i+3]+U[i+1])/di32/di21,
		 -1/di22/di21-1/di32/di21);
    p3[i]= dpol (square(U[i+3])/di32/di31,
		 -2*U[i+3]/di32/di31,
		 1/di32/di31);
    /*
    p1[i][2]= 1/di22/di11;
    p1[i][1]= -2*U[i]/di22/di11;
    p1[i][0]= square(U[i])/di22/di11;
    p2[i][2]= -1/di22/di21-1/di32/di21;
    p2[i][1]= (U[i+2]+U[i])/di22/di21+(U[i+3]+U[i+1])/di32/di21;
    p2[i][0]= -U[i+2]*U[i]/di22/di21-U[i+3]*U[i+1]/di32/di21;
    p3[i][2]= 1/di32/di31;
    p3[i][1]= -2*U[i+3]/di32/di31;
    p3[i][0]= square(U[i+3])/di32/di31;
    */
  }
  if (interpol) {
    array<point> x(n+1), y(n+1);
    y= a;
    {
      array<double> a(n+1), b(n+1), c(n+1);
      if (close) {
        a[n-2]= S (p1, p2, p3, n-3, m(n-1));
        b[0]= S (p1, p2, p3, 1, m(2));
        b[n-2]= S (p1, p2, p3, n-2, m(n-1));
        c[0]=S (p1, p2, p3, 2, m(2));
        for (i=1; i<n-2; i++) {
           a[i]= S (p1, p2, p3, i, m(i+2));
           b[i]= S (p1, p2, p3, i+1, m(i+2));
           c[i]= S (p1, p2, p3, i+2, m(i+2));
        }
        xtridiag_solve (a, b, c, S(p1, p2, p3, n-1, m(n-1)),
                                 S(p1, p2, p3, 0, m(2)),
                                 x, y, n-1);
     /* Rotate the result in order to have an appropriate
        parametrization (to keep in sync the ordering of
        the points & the paths in cip, in particular) */
        point p=x[n-2];
        for (i=n-2;i>=1;i--)
          x[i]=x[i-1];
        x[0]=p;
     // Splice the spline
        x[n-1]= x[0];
        x[n]= x[1];
      }
      else {
        a[n]= S (p1, p2, p3, n-1, U[n+1]);
        b[0]= S (p1, p2, p3, 0, U[2]);
        b[n]= S (p1, p2, p3, n, U[n+1]);
        c[0]= S (p1, p2, p3, 1, U[2]);
        for (i=1; i<n; i++) {
           a[i]= S (p1, p2, p3, i-1, m(i+1));
           b[i]= S (p1, p2, p3, i, m(i+1));
           c[i]= S (p1, p2, p3, i+1, m(i+1));
        }
        tridiag_solve (a, b, c, x, y, n+1);
      }
    }
    a= x;
  }
  for (i=2; i<=n; i++)
    p[i]= a[i]*p1[i] + a[i-1]*p2[i-1] + a[i-2]*p3[i-2];
}

// Evaluation
double
spline_rep::S (
  array<dpol> p1, array<dpol> p2, array<dpol> p3,
  int i, double u)
{
  if (i<0 || i>n) return 0;
  else if (u<U[i] || u>=U[i+3]) return 0;
  else if (u<U[i+1]) return p1[i](u);
  else if (u<U[i+2]) return p2[i](u);
  else if (u<U[i+3]) return p3[i](u);
  else FAILED ("we should **never** go here");
  return 0.0;
}

point
spline_rep::spline (int i, double u, int o) {
  int j, n= N(p[i]);
  point res (n);
  if (o<0 || o>2) o=0;
  for (j=0; j<n; j++)
    res[j]= p[i][j] (u, o);
  return res;
}

int
spline_rep::interval_no (double u) {
  int i;
  for (i=0;i<N(U);i++)
    if (u>=U[i] && u<U[i+1]) return i;
  return -1;
}

static double
prod (double x, int n) {
  double r;
  if (n<=0) return 1;
  r=x; n--;
  while (n--)
    r*=(x-n);
  return r;
}

point
spline_rep::evaluate (double t, int o) {
  point res;
  int no;
  t=convert(t);
  double k=U[n+1]-U[2];
  no=interval_no(t);
  if (no<2) res=spline(2,U[2],o);
  else if (no>n) res=spline(n,U[n+1],o);
  else res=spline(no,t,o);
  return prod(k,o)*res;
}

point
spline_rep::evaluate (double t) {
  return evaluate(t,0);
}

double
spline_rep::bound (double t, double eps) {
  return eps/norm(evaluate(t,1));
}

point
spline_rep::grad (double t, bool& error) {
  error= false;
  return evaluate(t,1);
}

// Rectification
bool
spline_rep::approx (int i, double u1, double u2, double eps) {
  double l,R;
  point p1,p2;
  p1=spline(i,u1);
  p2=spline(i,u2);
  l=norm(p1-p2);
  // When l and R are very small, the test l<=R
  // can fail forever. So we set l to exactly 0
  if (l!=0 && fnull (l,1.0e-6)) l=0;
  R=curvature(i,u1,u2);
  return l<=2*sqrt(2*R*eps);
}

void
spline_rep::rectify_cumul (array<point>& cum, int i,
                           double u1, double u2, double eps) {
  if (approx(i,u1,u2,eps))
    cum << spline(i,u2);
  else {
    double u=(u1+u2)/2;
    rectify_cumul(cum,i,u1,u,eps);
    rectify_cumul(cum,i,u,u2,eps);
  }
}

void
spline_rep::rectify_cumul (array<point>& cum, double eps) {
  int i;
  for (i=2;i<=n;i++)
    rectify_cumul(cum,i,U[i],U[i+1],eps);
}

// Curvature
double
spline_rep::curvature (int i, double t1, double t2) {
  point a, b;
  a= extract (p[i], 2);
  b= extract (p[i], 1);
  double t,R;
  point pp,ps;
  if (norm(a)==0) return tm_infinity;
  t=-inner (a, b) / (2 * inner (a, a));
  if (t1>t) t=t1;
  else if (t2<t) t=t2;
  pp=spline(i,t,1);
  ps=spline(i,t,2);
  if (norm(ps)==0) return tm_infinity;
  R=square(norm(pp))/norm(ps);
  return R;
}

double
spline_rep::curvature (double t1, double t2) {
  double res;
  int no,no1,no2;
  t1=convert(t1);
  t2=convert(t2);
  no1=interval_no(t1);
  no2=interval_no(t2);
  res=tm_infinity;
  for (no=no1;no<=no2;no++) {
    double r;
    if (no<2) r=curvature(2,t1,t2);
    else if (no>n) r=curvature(n,t1,t2);
    else r=curvature(no,t1,t2);
    if (r<=res) res=r;
  }
  return res;
}

// Control points
int
spline_rep::get_control_points (
  array<double>&abs, array<point>& pts, array<path>& rcip)
{
  array<double> u(n+1);
  array<point> p(n+1);
  int i;
  if (interpol) {
    if (close) {
      u->resize (n-1);
      p->resize (n-1);
      for (i=0; i<n-1; i++)
	u[i]= unconvert ((U[i+2]+U[i+3])/2);
    }
    else {
      u[0]= unconvert (U[2]);
      u[n]= unconvert (U[n+1]);
      for (i=2; i<=n; i++)
	u[i-1]= unconvert ((U[i]+U[i+1])/2);
    }
    for (i=0; i<=(close ? n-2 : n); i++)
      p[i]= evaluate (u[i]);
  }
  else FAILED ("not yet implemented");
  abs = u;
  pts = p;
  rcip= cip;
  return close ? n-1 : n+1;
}

curve
spline (array<point> a, array<path> cip, bool close, bool interpol) {
  return tm_new<spline_rep> (a, cip, close, interpol);
}

/******************************************************************************
* Bezier curves
******************************************************************************/

struct bezier_rep: public curve_rep {
  array<point> a;
  array<point> P;
  bezier_rep (array<point> a);
  point evaluate (double t);
  void rectify_cumul (array<point>& cum, double t0, double t1, double eps);
  void rectify_cumul (array<point>& cum, double eps);
  double bound (double t, double eps);
  point grad (double t, bool& error);
  double curvature (double t1, double t2);
};

bezier_rep::bezier_rep (array<point> a2): a (a2) {
  P << a[0];
  P << (-3.0*a[0] + 3.0*a[1]);
  P << (3.0*a[0] - 6.0*a[1] + 3.0*a[2]);
  P << (-a[0] + 3.0*a[1] - 3.0*a[2] + a[3]);
}

point
bezier_rep::evaluate (double t) {
  return ((P[3]*t + P[2])*t + P[1])*t + P[0];
}

void
bezier_rep::rectify_cumul (array<point>& cum, double t0, double t1, double e) {
  point p0= evaluate (t0);
  point p1= evaluate (t1);
  //if (bound ((t0 + t1) / 2.0, e / 4.0) < (t1 - t0))
    for (int k=1; k<=4; k++) {
      double x= ((double) k) / 5.0;
      double t= (1.0 - x) * t0 + x * t1;
      point  q= evaluate (t);
      point  r= (1.0 - x) * p0 + x * p1;
      if (norm (q - r) >= (e / 10.0)) {
	rectify_cumul (cum, t0, (t0 + t1) / 2.0, e);
	rectify_cumul (cum, (t0 + t1) / 2.0, t1, e);
	return;
      }
    }
  cum << p1;
}

void
bezier_rep::rectify_cumul (array<point>& cum, double eps) {
  rectify_cumul (cum, 0.0, 1.0, eps);
}

double
bezier_rep::bound (double t, double eps) {
  (void) t;
  double K= 3.0*norm(P[3]) + 2.0*norm(P[2]) + norm(P[1]);
  return min (eps / (K + 1.0e-6), 1.0);
}

point
bezier_rep::grad (double t, bool& error) {
  error= false;
  return ((3.0*P[3]*t) + 2.0*P[2]) + P[1];
}

double
bezier_rep::curvature (double t1, double t2) {
  (void) t1; (void) t2;
  return tm_infinity; // FIXME
}

curve
bezier (array<point> a) {
  return tm_new<bezier_rep> (a);
}

/******************************************************************************
* Poly-Bezier curves
******************************************************************************/

struct poly_bezier_rep: public curve_rep {
  double phi;
  array<point> a;
  array<path> cip;
  curve wrap;
  bool simple;
  bool closed;
  poly_bezier_rep (array<point> a, array<path> cip, bool simple, bool closed);
  point evaluate (double t);
  void rectify_cumul (array<point>& cum, double eps);
  double bound (double t, double eps);
  point grad (double t, bool& error);
  double curvature (double t1, double t2);
  int get_control_points (
    array<double>&abs, array<point>& pts, array<path>& cip);
};

poly_bezier_rep::poly_bezier_rep (array<point> a2, array<path> cip2,
				  bool simple2, bool closed2):
  phi (3.0), a (a2), cip (cip2), simple (simple2), closed (closed2)
{
  array<point> p;
  if (simple) {
    array<point> b;
    int di= (closed? 0: 1);
    for (int i=di; i<N(a)-di; i++) {
      point p0= a[(i+N(a)-1) % N(a)];
      point p1= a[i];
      point p2= a[(i+1) % N(a)];
      if (norm (p2 - p0) < 1.0e-6) {
	b << (p0 + (phi - 1.0) * p1) / phi;
	b << p1;
	b << ((phi - 1.0) * p1 + p2) / phi;
      }
      else {
	point u= (p2 - p0) / norm (p2 - p0);
	double l1= norm (p1 - p0) / phi;
	double l2= norm (p2 - p1) / phi;
	b << p1 - l1 * u;
	b << p1;
	b << p1 + l2 * u;
      }
    }
    if (closed) {
      p << range (b, 1, N(b));
      p << range (b, 0, 2);
    }
    else if (N(a) == 2) {
      p << a[0];
      p << ((phi - 1.0) * a[0] + a[1]) / phi;
      p << (a[0] + (phi - 1.0) * a[1]) / phi;
      p << a[1];
    }
    else {
      p << a[0];
      if (norm (b[0] - a[0]) < 1.0e-6) p << b[0];
      else {
	double l1= norm (a[1] - a[0]) / phi;
	double l2= norm (b[0] - a[0]);
	p << a[0] + (b[0] - a[0]) * (l1 / l2);
      }
      p << b;
      if (norm (a[N(a)-1] - b[N(b)-1]) < 1.0e-6) p << b[N(b)-1];
      else {
	double l1= norm (a[N(a)-1] - a[N(a)-2]) / phi;
	double l2= norm (b[N(b)-1] - a[N(a)-1]);
	p << a[N(a)-1] + (b[N(b)-1] - a[N(a)-1]) * (l1 / l2);
      }
      p << a[N(a)-1];
    }
  }
  else {
    p= copy (a);
    if (closed) p << a[0];
    if (((N(p) - 1) % 3) == 1) {
      p << p[N(p)-1];
      p[N(p)-2]= p[N(p)-3] + (p[N(p)-3] - p[N(p)-4]);
    }
    if (((N(p) - 1) % 3) == 2) {
      p << p[N(p)-1];
      p[N(p)-2]= p[N(p)-3];
    }
  }
  array<curve> cs;
  for (int i=0; (i+3)<N(p); i+=3) {
    array<point> b;
    b << p[i] << p[i+1] << p[i+2] << p[i+3];
    cs << bezier (b);
  }
  wrap= compound (cs);
}

point
poly_bezier_rep::evaluate (double t) {
  return wrap->evaluate (t);
}

void
poly_bezier_rep::rectify_cumul (array<point>& cum, double eps) {
  wrap->rectify_cumul (cum, eps);
}

double
poly_bezier_rep::bound (double t, double eps) {
  return wrap->bound (t, eps);
}

point
poly_bezier_rep::grad (double t, bool& error) {
  return wrap->grad (t, error);
}

double
poly_bezier_rep::curvature (double t1, double t2) {
  return wrap->curvature (t1, t2);
}

int
poly_bezier_rep::get_control_points (
  array<double>&abs, array<point>& pts, array<path>& rcip)
{
  abs = array<double> ();
  pts = a;
  rcip= cip;
  if (simple) {
    int n= N(a);
    int k= (closed? n: n-1);
    for (int i=0; i<n; i++)
      abs << ((double) i) / ((double) k);
  }
  else {
    int n= N(a);
    if (!closed) n--;
    int k= 3*((n+2)/3);
    for (int i=0; i<n; i++)
      abs << ((double) i) / ((double) k);
    if (!closed) abs << 1.0;
  }
  return N(a);
}

curve
poly_bezier (array<point> a, array<path> cip, bool simple, bool closed) {
  return tm_new<poly_bezier_rep> (a, cip, simple, closed);
}

/******************************************************************************
* Arcs
******************************************************************************/

struct arc_rep: public curve_rep {
  array<point> a;
  array<path> cip;
  array<double> u;
  point center;  // Center
  point i, j;    // The two base vectors of the ellipsis's 2D plane
  double r1, r2; // The two radiuses of the ellipsis
  double e1, e2; // Coordinates of the two extremal points of the arc
  arc_rep (array<point> a, array<path> cip, bool close);
  point evaluate (double t);
  void rectify_cumul (array<point>& cum, double eps);
  double bound (double t, double eps);
  point grad (double t, bool& error);
  double curvature (double t1, double t2);
  int get_control_points (
    array<double>&abs, array<point>& pts, array<path>& cip);
};

arc_rep::arc_rep (array<point> a2, array<path> cip2, bool close):
  a(a2), cip(cip2)
{
  int n= N(a);
  point o1= (n>0 ? a[0] : point (0,0));
  point o2= (n>1 ? a[1] : point (0,0));
  point o3= (n>2 ? a[2] : point (0,0));
  if (n!=3 || linearly_dependent (o1, o2, o3) ||
     (N (center= intersection (midperp (o1, o2, o3),
			       midperp (o2, o3, o1))) == 0))
  {
    center= i= j= 0*o1;
    r1= r2= 1;
    e1= 0;
    e2= 1;
    a= array<point> (1);
    a[0]= o1;
    if (N(cip)) {
      path p= cip[0];
      cip= array<path> (1);
      cip[0]= p;
    }
    u= array<double> (1);
    u[0]= 0;
    return;
  }
  r1= r2= norm (o1-center);
  if (orthogonalize (i, j, center, o1, o2)) ;
  else
    orthogonalize (i, j, center, o1, o3);
  e1= 0;
  point o2b (2), o3b (2);
  o2b[0]= inner (o2-center, i);
  o2b[1]= inner (o2-center, j);
  o3b[0]= inner (o3-center, i);
  o3b[1]= inner (o3-center, j);
  e2= arg (o3b) / (2*tm_PI);
  double e3= arg (o2b) / (2*tm_PI);
  if (e2<e3) {
    j= -j;
    o3b[1]= -o3b[1];
    e2= arg (o3b) / (2*tm_PI);
    o2b[1]= -o2b[1];
    e3= arg (o2b) / (2*tm_PI);
  }
  u= array<double> (3);
  u[0]= 0;
  u[1]= e3;
  u[2]= e2;
  if (close) e2= 1.0;
}

point
arc_rep::evaluate (double t) {
  t= e1 + t*(e2 - e1);
  return center + r1*cos(2*tm_PI*t)*i
                + r2*sin(2*tm_PI*t)*j;
}

void
arc_rep::rectify_cumul (array<point>& cum, double eps) {
  double t, step;
  step= sqrt (2*eps / max (r1, r2) ) / tm_PI;
  for (t=step; t<=1.0; t+=step)
    cum << evaluate (t);
  if (t-step != 1.0)
    cum << evaluate (1.0);
}

double
arc_rep::bound (double t, double eps) {
  return curve_rep::bound (t, eps);
}

point
arc_rep::grad (double t, bool& error) {
  error= false;
  t= e1 + t*(e2 - e1);
  return -2*tm_PI*r1*sin(2*tm_PI*t)*i
         +2*tm_PI*r2*cos(2*tm_PI*t)*j;
}

double
arc_rep::curvature (double t1, double t2) {
  (void) t1; (void) t2;
  if (r1 >= r2)
    return fnull (r1,1.0e-6) ? tm_infinity : square(r2)/r1;
  else
    return fnull (r2,1.0e-6) ? tm_infinity : square(r1)/r2;
}

curve
arc (array<point> a, array<path> cip, bool close) {
  return tm_new<arc_rep> (a, cip, close);
}

int
arc_rep::get_control_points (
  array<double>&abs, array<point>& pts, array<path>& rcip)
{
  abs = u;
  pts = a;
  rcip= cip;
  return N(a);
}

/******************************************************************************
* Compound curves
******************************************************************************/

struct compound_curve_rep: public curve_rep {
  array<curve> cs;
  compound_curve_rep (array<curve> cs2): cs (cs2) {}
  int nr_components () {
    int tot= 0;
    for (int i=0; i<N(cs); i++)
      tot += cs[i]->nr_components ();
    return tot; }
  int get_index (double t) {
    int i= (int) floor (N(cs) * t);
    if (i < 0) return 0;
    if (i >= N(cs)) return N(cs) - 1;
    return i;
  }
  double get_subtime (double t) {
    int i= (int) floor (N(cs) * t);
    double res= (N(cs) * t) - floor (N(cs) * t);
    if (i < 0) return 0.0;
    if (i >= N(cs)) return 1.0;
    return res;
  }
  point evaluate (double t) {
    return cs [get_index (t)]->evaluate (get_subtime (t)); }
  void rectify_cumul (array<point>& a, double eps) {
    for (int i=0; i<N(cs); i++)
      cs[i]->rectify_cumul (a, eps);
  }
  double bound (double t, double eps) {
    double st= get_subtime (t);
    double dt= cs [get_index (t)]->bound (st, eps);
    dt= min (min (dt, st + 1.0e-6), 1.0 + 1.0e-6 - st);
    return dt / N(cs);
  }
  point grad (double t, bool& error) {
    int i= get_index (t);
    point g= cs[i]->grad (get_subtime (t), error);
    if (get_subtime (t) <= 1.0e-6 && i > 0) {
      point g2= cs[i-1]->grad (1.0, error);
      if (norm (g2 - g) >= 1.0e-6) error= true;
    }
    return N(cs) * g;
  }
  double curvature (double t1, double t2) {
    int i1= get_index (t1);
    int i2= get_index (t2);
    double u1= get_subtime (t1);
    double u2= get_subtime (t2);
    double m= 0.0;
    if (i1 == i2)
      m= cs[i1]->curvature (u1, u2);
    else
      for (int i=i1; i<=i2; i++) {
	if (i == i1) m= max (m, cs[i]->curvature (u1, 1.0));
	else if (i == i2) m= max (m, cs[i]->curvature (0.0, u2));
	else m= max (m, cs[i]->curvature (0.0, 1.0));
      }
    return m;
  }
};

curve
compound (array<curve> a) {
  // FIXME: we might want to test for continuity at the junctions
  if (N(a) == 0) FAILED ("at least one curve expected");
  if (N(a) == 1) return a[0];
  return tm_new<compound_curve_rep> (a);
}

/******************************************************************************
* Inverted curves
******************************************************************************/

struct inverted_curve_rep: public curve_rep {
  curve c;
  int n;
  inverted_curve_rep (curve c2): c (c2), n (c->nr_components()) {}
  int nr_components () { return n; }
  point evaluate (double t) { return c (1.0 - t); }
  void rectify_cumul (array<point>& a, double eps) {
    array<point> b= c->rectify (eps);
    int i, k= N(b);
    for (i=k-1; i>=0; i--) a << b[i];
  }
  double bound (double t, double eps) {
    return curve_rep::bound (t, eps);
  }
  point grad (double t, bool& error) {
    return - c->grad (1.0 - t, error);
  }
  double curvature (double t1, double t2) {
    return c->curvature (1-t2, 1-t1);
  }
  int get_control_points (
    array<double>&abs, array<point>& pts, array<path>& cip);
};

int
inverted_curve_rep::get_control_points (
  array<double>&abs, array<point>& pts, array<path>& cip)
{
  int res= c->get_control_points (abs, pts, cip);
  int i;
  abs= copy (abs);
  for (i=0; i<res; i++)
    abs[i]= 1 - abs[i];
  return res;
}

curve
invert (curve c) {
  return tm_new<inverted_curve_rep> (c);
}

/******************************************************************************
* Transformed curves
******************************************************************************/

struct transformed_curve_rep: public curve_rep {
  frame f;
  curve c;
  int n;
  transformed_curve_rep (frame f2, curve c2):
    f (f2), c (c2), n (c->nr_components()) {}
  int nr_components () { return n; }
  point evaluate (double t) { return f (c (t)); }
  void rectify_cumul (array<point>& a, double eps);
  double bound (double t, double eps) {
    return curve_rep::bound (t, eps);
  }
  point grad (double t, bool& error);
  double curvature (double t1, double t2) {
    (void) t1; (void) t2;
    FAILED ("not yet implemented");
    return 0.0;
  }
  int get_control_points (
    array<double>&abs, array<point>& pts, array<path>& cip);
};

void
transformed_curve_rep::rectify_cumul (array<point>& a, double eps) {
  if (f->linear) {
    double delta= f->direct_bound (c(0.0), eps);
    array<point> b= c->rectify (delta);
    int i, k= N(b);
    for (i=0; i<k; i++) a << f(b[i]);
  }
  else FAILED ("not yet implemented");
}

point
transformed_curve_rep::grad (double t, bool& error) {
  bool error2;
  point w2= c->grad (t, error2);
  point w1= f->jacobian (c(t), w2, error);
  error |= error2;
  return w1;
}

int
transformed_curve_rep::get_control_points (
  array<double>&abs, array<point>& pts, array<path>& cip)
{
  int res= c->get_control_points (abs, pts, cip);
  int i;
  pts= copy (pts);
  for (i=0; i<N(pts); i++)
    pts[i]= f (pts[i]);
  return res;
}

curve
frame::operator () (curve c) {
  return tm_new<transformed_curve_rep> (*this, c);
}

curve
frame::operator [] (curve c) {
  return tm_new<transformed_curve_rep> (invert (*this), c);
}

/******************************************************************************
* Portions of curves
******************************************************************************/

void
poor_rectify_cumul (curve c, array<point>& a, double eps,
                    array<double>& ts, double t0, double t1) {
  int NUM= 5;
  point p0= c->evaluate (t0);
  point p1= c->evaluate (t1);
  for (int i=1; i<NUM; i++) {
    double u= ((double) i) / ((double) NUM);
    point pi = c->evaluate (t0 + u * (t1 - t0));
    point pi2= p0 + u * (p1 - p0);
    if (norm (pi2 - pi) >= eps) {
      poor_rectify_cumul (c, a, eps, ts, t0, (t0 + t1) / 2);
      poor_rectify_cumul (c, a, eps, ts, (t0 + t1) / 2, t1);
      return;
    }
  }
  a  << p1;
  ts << t1;
}

struct partial_curve_rep: public curve_rep {
  curve c;
  double t0, t1;
  partial_curve_rep (curve c2, double t0b, double t1b):
    c (c2), t0 (t0b), t1 (t1b) {}
  int nr_components () { return c->nr_components (); }
  point evaluate (double t) { return c (t0 + (t1 - t0) * t); }
  void rectify_cumul (array<point>& a, double eps) {
    array<double> ts;
    poor_rectify_cumul (c, a, eps, ts, t0, t1); }
  double bound (double t, double eps) {
    return c->bound (t0 + (t1 - t0) * t, eps) / (t1 - t0); }
  point grad (double t, bool& error) {
    return (t1 - t0) * c->grad (t0 + (t1 - t0) * t, error); }
  double curvature (double u1, double u2) {
    return c->curvature (t0 + (t1 - t0) * u1, t0 + (t1 - t0) * u2); }
  int get_control_points (
    array<double>&abs, array<point>& pts, array<path>& cip);
  array<double> find_closest_points (
    double t1, double t2, point p, double eps);
};

int
partial_curve_rep::get_control_points (
  array<double>&abs, array<point>& pts, array<path>& cip)
{
  int res= c->get_control_points (abs, pts, cip);
  int i;
  abs= copy (abs);
  for (i=0; i<res; i++)
    abs[i]= (abs[i] - t0) / (t1 - t0);
  return res;
}


array<double>
partial_curve_rep::find_closest_points (
  double t1, double t2, point p, double eps)
{
  if (t1 > t2) { double a= t1; t1= t2; t2= a; }
  return curve_rep::find_closest_points (t1, t2, p, eps);
}

curve
part (curve c, double start, double end) {
  ASSERT (fabs (end - start) > 1.0e-6, "very small portions not supported");
  return tm_new<partial_curve_rep> (c, start, end);
}

curve
truncate (curve c, double portion, double eps) {
  if (portion == 1.0) return c;
  ASSERT (portion > 1.0e-6 && portion < 1, "invalid portion of curve");
  array<point>  a;
  array<double> ts;
  a  << c->evaluate (0.0);
  ts << 0.0;
  poor_rectify_cumul (c, a, eps, ts, 0.0, 1.0);
  double tot= 0.0;
  for (int i=1; i<N(a); i++)
    tot += norm (a[i] - a[i-1]);
  double rem= portion * tot;
  for (int i=1; i<N(a); i++) {
    double d= norm (a[i] - a[i-1]);
    if (d > rem) {
      double t0= ts[i-1];
      double t1= ts[i];
      double a = rem / d;
      double tt= t0 + a * (t1 - t0);
      return part (c, 0.0, tt);
    }
    else rem -= d;
  }
  return c;
}

/******************************************************************************
* Changing the control points
******************************************************************************/

struct recontrol_curve_rep: public curve_rep {
  curve c;
  array<point> a;
  array<path> cip;
  recontrol_curve_rep (curve c2, array<point> a2, array<path> cip2):
    c (c2), a (a2), cip (cip2) {}
  int nr_components () { return c->nr_components (); }
  point evaluate (double t) { return c (t); }
  void rectify_cumul (array<point>& a, double e) { c->rectify_cumul (a, e); }
  double bound (double t, double eps) { return c->bound (t, eps); }
  point grad (double t, bool& error) { return c->grad (t, error); }
  double curvature (double u1, double u2) { return c->curvature (u1, u2); }
  int get_control_points (
    array<double>&abs, array<point>& a2, array<path>& cip2) {
      a2= a;
      cip2= cip;
      int i, n= N(a);
      abs= array<double> (n);
      for (i=0; i<n; i++) abs[i]= i / (1.0 * max (1, n-1));
      return n; }
  array<double> find_closest_points (
    double t1, double t2, point p, double eps) {
      return c->find_closest_points (t1, t2, p, eps); }
};

curve
recontrol (curve c, array<point> a, array<path> cip) {
  return tm_new<recontrol_curve_rep> (c, a, cip);
}
