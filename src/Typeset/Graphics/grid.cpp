
/******************************************************************************
* MODULE     : grid.cpp
* DESCRIPTION: grids for the graphics
* COPYRIGHT  : (C) 2003  Henri Lesourd
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "grid.hpp"
#include "math_util.hpp"
#include "curve.hpp"

/******************************************************************************
* General routines
******************************************************************************/

void
grid_rep::set_aspect (tree aspect) {
  subd= array<SI> (2);
  subd[0]= 0;
  subd[1]= 1;
  col= array<string> (2);
  col[0]= "#808080";
  col[1]= "#c0c0c0";
  if (is_tuple (aspect)) {
    int i;
    bool b= false;
    subd= array<SI> (N(aspect));
    col= array<string> (N(aspect));
    for (i=0; i<N(aspect); i++) {
       if (is_tuple (aspect[i], "axes", 1)) {
         subd[i]= 0;
         b= true;
       }
       else {
         subd[i]= as_int (aspect[i][0]);
       }
       col[i]= as_string (aspect[i][1]);
    }
    if (!b) {
      array<SI> subd0 (1);
      array<string> col0 (1);
      subd0[0]= 0;
      col0[0]= "#e0e0ff";
      subd= subd0 << subd;
      col= col0 << col;
    }
    do {
      b= true;
      for (i=1; i<N(subd); i++)
         if (subd[i-1]>subd[i]) {
           SI j;
           string c;
           j= subd[i-1];subd[i-1]= subd[i];subd[i]= j;
           c= col[i-1];col[i-1]= col[i];col[i]= c;
           b= false;
         }
    }
    while (!b);
  }
}

array<grid_curve>
grid_rep::get_curves_around (point p, double delta, frame f) {
  point p2= f (p);
  point pmin, pmax;
  pmin= f[point (p2[0]-delta, p2[1]-delta)];
  pmax= f[point (p2[0]+delta, p2[1]+delta)];
  return get_curves (pmin, pmax, 1e-6, true);
}

point
grid_rep::find_point_around (point p, double delta, frame f) {
  point p2= f (p);
  point pmin, pmax;
  pmin= f[point (p2[0]-delta, p2[1]-delta)];
  pmax= f[point (p2[0]+delta, p2[1]+delta)];
  return find_closest_point (p, pmin, pmax);
}

/******************************************************************************
* The empty grid
******************************************************************************/

struct empty_grid_rep: public grid_rep {
  empty_grid_rep ():
    grid_rep () {}
  array<grid_curve> get_curves (point lim1, point lim2, double u, bool b) {
    (void) lim1; (void) lim2; (void) u; (void) b;
    array<grid_curve> res;
    return res; }
  operator tree () { return "empty_grid"; }
  point find_closest_point (point p, point pmin, point pmax);
};

point
empty_grid_rep::find_closest_point (point p, point pmin, point pmax) {
  (void) pmin; (void) pmax;
/*double x= floor (10.0*p[0] + 0.5);
  double y= floor (10.0*p[1] + 0.5);
  return point (x / 10.0, y / 10.0);*/
  return p;
}

grid
empty_grid () {
  return tm_new<empty_grid_rep> ();
}

/******************************************************************************
* Cartesian grids
******************************************************************************/

struct cartesian_rep: public grid_rep {
  double step; // Unit length
  cartesian_rep (array<SI> subd, array<string> col, point o, double st):
    grid_rep (subd, col, o), step (st) {}
  operator tree () { return "cartesian"; }
  array<grid_curve> get_curves (point lim1, point lim2, double u, bool b);
  array<grid_curve> get_curves_around (point p, double delta, frame f);
  point find_closest_point (point p, point pmin, point pmax);
};

static grid_curve
create_line (double x1, double y1, double x2, double y2, string col) {
  array<point> a(2);
  a[0]= point (x1, y1);
  a[1]= point (x2, y2);
  array<path> cip(2);
  grid_curve res= grid_curve (col, poly_segment (a, cip));
  return res;
}

array<grid_curve>
cartesian_rep::get_curves (point lim1, point lim2, double u, bool b) {
  array<grid_curve> res;
  if (N(subd)<1) return res;
  double x1= min (lim1[0], lim2[0]);
  double y1= min (lim1[1], lim2[1]);
  double x2= max (lim1[0], lim2[0]);
  double y2= max (lim1[1], lim2[1]);
  double xo= center[0];
  double yo= center[1];
  int i;
  for (i= N(subd)-1; i>=1; i--) {
     SI nsub;
     nsub= subd[i];
     if (nsub!=0) {
       double x, y, s;
       s= step/nsub;
       if (s<=u) s= u;
       for (x= xo; x<=x2; x+=s)
         if (x>=x1) {
           res << create_line (x, y1, x, y2, col[i]);
           if (b) break;
         }
       for (x= xo-s; x>=x1; x-=s)
         if (x<=x2) {
           res << create_line (x, y1, x, y2, col[i]);
           if (b) break;
         }
       for (y= yo; y<=y2; y+=s)
         if (y>=y1) {
           res << create_line (x1, y, x2, y, col[i]);
           if (b) break;
         }
       for (y= yo-s; y>=y1; y-=s)
         if (y<=y2) {
           res << create_line (x1, y, x2, y, col[i]);
           if (b) break;
         }
     }
  }
  if (!b && yo>=y1 && yo<=y2)
    res << create_line (x1, yo, x2, yo, col[0]);
  if (!b && xo>=x1 && xo<=x2)
    res << create_line (xo, y1, xo, y2, col[0]);
  return res;
}

array<grid_curve>
cartesian_rep::get_curves_around (point p, double delta, frame f) {
  double nsub= 0;
  string c;
  for (int i=0; i<N(subd); i++)
    if (subd[i] != 0) { nsub= subd[i]; c= col[i]; }
  if (nsub == 0) return array<grid_curve> (0);

  point p2  = f (p);
  point lim1= f[point (p2[0]-delta, p2[1]-delta)];
  point lim2= f[point (p2[0]+delta, p2[1]+delta)];
 
  double x1= min (lim1[0], lim2[0]);
  double y1= min (lim1[1], lim2[1]);
  double x2= max (lim1[0], lim2[0]);
  double y2= max (lim1[1], lim2[1]);
  double xo= center[0];
  double yo= center[1];
  double s = step / nsub;

  double X1= xo + floor ((p[0] - xo) / s) * s;
  double Y1= yo + floor ((p[1] - yo) / s) * s;
  double X2= xo + ceil  ((p[0] - xo) / s) * s;
  double Y2= yo + ceil  ((p[1] - yo) / s) * s;
  x1= min (x1, X1 - s / 10);
  y1= min (y1, Y1 - s / 10);
  x2= max (x2, X2 + s / 10);
  y2= max (y2, Y2 + s / 10);

  array<grid_curve> res;
  res << create_line (X1, y1, X1, y2, c);
  res << create_line (x1, Y1, x2, Y1, c);
  if (X2 > X1) res << create_line (X2, y1, X2, y2, c);
  if (Y2 > Y1) res << create_line (x1, Y2, x2, Y2, c);
  return res;
}

point
cartesian_rep::find_closest_point (point p, point pmin, point pmax) {
  double x, y, oldx=0, oldy= 0, ssubd;
  point res= p;
  p= p-center;
  int i;
  for (i=1; i<N(subd); i++) {
    ssubd= ((double) subd[i]) / step;
    if (ssubd==0) continue;
    x= nearest (p[0]*ssubd);
    y= nearest (p[1]*ssubd);
    res= center + point (x/ssubd, y/ssubd);
    if (i!=1) {
      if (inside_rectangle (point (oldx, res[1]), pmin, pmax))
        return point (oldx, res[1]);
      if (inside_rectangle (point (res[0], oldy), pmin, pmax))
        return point (res[0], oldy);
    }
    oldx= res[0];
    oldy= res[1];
    if (inside_rectangle (res, pmin, pmax))
      return res;
  }
  return res;
}

grid
cartesian (array<SI> subd, array<string> col, point o, double step) {
  return tm_new<cartesian_rep> (subd, col, o, step);
}

/******************************************************************************
* Polar grids
******************************************************************************/

struct polar_rep: public grid_rep {
  double step;  // Radial unit length
  SI astep;     // # angles
  polar_rep (array<SI> subd, array<string> col, point o, double st, SI ast):
    grid_rep (subd, col, o), step (st), astep (ast) {}
  operator tree () { return "polar"; }
  array<grid_curve> get_curves (point lim1, point lim2, double u, bool b);
  point find_closest_point (point p, point pmin, point pmax);
};

static grid_curve
create_arc (
  double x1, double y1, double x2, double y2, double x3, double y3, string col)
{
  array<point> a(3);
  a[0]= point (x1, y1);
  a[1]= point (x2, y2);
  a[2]= point (x3, y3);
  array<path> cip(3);
  grid_curve res= grid_curve (col, arc (a, cip, true));
  return res;
}

array<grid_curve>
polar_rep::get_curves (point lim1, point lim2, double u, bool b) {
  (void) b;
  array<grid_curve> res;
  if (N(subd)<1) return res;
  double x1= min (lim1[0], lim2[0]);
  double y1= min (lim1[1], lim2[1]);
  double x2= max (lim1[0], lim2[0]);
  double y2= max (lim1[1], lim2[1]);
  double xo= center[0];
  double yo= center[1];
  point P1, P2;
  if (x1<=0 && y1<=0 && x2>=0 && y2>=0) {
    P1= point (0, 0);
    P2= point (x2, y2) - point (x1, y1);
  }
  else {
    double ox= (x1 + x2) / 2;
    double oy= (y1 + y2) / 2;
    if (oy>=0)
      P1= point (0, y1>=0 ? y1 : 0);
    else
      P1= point (0, y2<=0 ? y2 : 0);

    if (ox>=0 && oy>=0)
      P2= point (x2, y2);
    else
    if (ox<=0 && oy>=0)
      P2= point (x1, y2);
    else
    if (ox<=0 && oy<=0)
      P2= point (x1, y1);
    else
    if (ox>=0 && oy<=0)
      P2= point (x2, y1);
  }
  double r, R1= norm (P1), R2= norm (P2);
  int i;
  for (i= N(subd)-1; i>=1; i--) {
    SI nsub;
    nsub= subd[i];
    if (nsub!=0) {
      SI j;
      double s= step/nsub;
      if (s<=u) s= u;
      for (r=0; r<=R2; r+=s)
	if (r>=R1)
	  res << create_arc (xo+r, yo, xo, yo+r, xo-r, yo, col[i]);
      for (j=0; j<astep*nsub; j++)
        res << create_line (xo, yo, xo+R2*cos((2*tm_PI*j)/(astep*nsub)),
                                    yo+R2*sin((2*tm_PI*j)/(astep*nsub)),
                                    col[i]);
    }
  }
  res << create_line (x1, yo, x2, yo, col[0]);
  res << create_line (xo, y1, xo, y2, col[0]);
  return res;
}

point
polar_rep::find_closest_point (point p, point pmin, point pmax) {
  double n, a, oldn= 0, olda= 0, ssubd;
  point res= p;
  p= p-center;
  int i;
  for (i=1; i<N(subd); i++) {
    ssubd= (double)subd[i];
    if (ssubd==0) continue;
    n= nearest (norm(p)*(ssubd/step));
    if (fnull (norm (p), 1.0e-6))
      a= 0.0;
    else
      a= nearest ((arg(p)/(2*tm_PI))*astep*ssubd);
    n= n*(step/ssubd);
    a= a/(astep*ssubd);
    if (i!=1) {
      res= center + oldn * point (cos(2*tm_PI*a), sin(2*tm_PI*a));
      if (inside_rectangle (res, pmin, pmax))
        return res;
      res= center + n * point (cos(2*tm_PI*olda), sin(2*tm_PI*olda));
      if (inside_rectangle (res, pmin, pmax))
        return res;
    }
    oldn= n;
    olda= a;
    res= center + n * point (cos(2*tm_PI*a), sin(2*tm_PI*a));
    if (inside_rectangle (res, pmin, pmax))
      return res;
  }
  return res;
}

grid
polar (array<SI> subd, array<string> col, point o, double step, SI astep) {
  return tm_new<polar_rep> (subd, col, o, step, astep);
}

/******************************************************************************
* Logarithmic grids
******************************************************************************/

struct logarithmic_rep: public grid_rep {
  double step; // Unit length
  SI base;
  logarithmic_rep (array<SI> subd, array<string> col, point o, double st, SI b):
    grid_rep (subd, col, o), step (st), base (b) {}
  operator tree () { return "logarithmic"; }
  array<grid_curve> get_curves (point lim1, point lim2, double u, bool b);
  point find_closest_point (point p, point pmin, point pmax);
};

array<grid_curve>
logarithmic_rep::get_curves (point lim1, point lim2, double u, bool b) {
  (void) b;
  array<grid_curve> res;
  if (N(subd)<1) return res;
  double x1= min (lim1[0], lim2[0]);
  double y1= min (lim1[1], lim2[1]);
  double x2= max (lim1[0], lim2[0]);
  double y2= max (lim1[1], lim2[1]);
  double xo= center[0];
  double yo= center[1];
  int i;
  double x, y;
  if (step<=u) step= u;
  if (N(col)>=3) {
    for (i=2; i<base; i++) {
      double dx, dy;
      dx= dy= step*log((double)i)/log((double)base);
      for (x=xo; x<=x2; x+=step)
        res << create_line (x+dx, y1, x+dx, y2, col[2]);
      for (x=xo-step; x>=x1-step; x-=step)
        res << create_line (x+dx, y1, x+dx, y2, col[2]);
      for (y=yo; y<=y2; y+=step)
        res << create_line (x1, y+dy, x2, y+dy, col[2]);
      for (y=yo-step; y>=y1-step; y-=step)
        res << create_line (x1, y+dy, x2, y+dy, col[2]);
    }
  }
  if (N(col)>=2) {
    for (x=xo; x<=x2; x+=step)
      res << create_line (x, y1, x, y2, col[1]);
    for (x=xo; x>=x1; x-=step)
      res << create_line (x, y1, x, y2, col[1]);
    for (y=yo; y<=y2; y+=step)
      res << create_line (x1, y, x2, y, col[1]);
    for (y=yo; y>=y1; y-=step)
      res << create_line (x1, y, x2, y, col[1]);
  }
  res << create_line (x1, yo, x2, yo, col[0]);
  res << create_line (xo, y1, xo, y2, col[0]);
  return res;
}

point
logarithmic_rep::find_closest_point (point p, point pmin, point pmax) {
  double x, y, ssubd= ((double) subd[1]) / step;
  point res= p;
  if (ssubd!=0) {
    p= p-center;
    x= nearest (p[0]*ssubd);
    y= nearest (p[1]*ssubd);
    res= center + point (x/ssubd, y/ssubd);
    if (inside_rectangle (res, pmin, pmax))
      return res;
    p= center+p;
  }
  double xo, yo;
  xo= center[0];
  yo= center[1];
  double x1, y1, x2, y2;
  x1= xo-step;
  y1= yo-step;
  x2= xo+step;
  y2= yo+step;
  p= p-center;
  double x0, y0;
  x0= (SI)(p[0]/step);
  y0= (SI)(p[1]/step);
  x0*= step;
  y0*= step;
  p= p-point(x0,y0);
  p= center+p;
  double xm, ym;
  xm= ym= tm_infinity/2;
  int i;
  for (i=1; i<base; i++) {
    double dx, dy;
    dx= dy= step*log((double)i)/log((double)base);
    for (x=xo; x<=x2; x+=step)
      if (norm(x+dx-p[0])<norm(xm-p[0])) xm= x+dx;
    for (x=xo-step; x>=x1-step; x-=step)
      if (norm(x+dx-p[0])<norm(xm-p[0])) xm= x+dx;
    for (y=yo; y<=y2; y+=step)
      if (norm(y+dy-p[1])<norm(ym-p[1])) ym= y+dy;
    for (y=yo-step; y>=y1-step; y-=step)
      if (norm(y+dy-p[1])<norm(ym-p[1])) ym= y+dy;
  }
  p= point (x0+xm, y0+ym);
  if (ssubd!=0) {
    if (inside_rectangle (point (res[0], p[1]), pmin, pmax))
      return point (res[0], p[1]);
    if (inside_rectangle (point (p[0], res[1]), pmin, pmax))
      return point (p[0], res[1]);
  }
  return p;
}

grid
logarithmic (array<SI> subd, array<string> col, point o, double step, SI base) {
  return tm_new<logarithmic_rep> (subd, col, o, step, base);
}

/******************************************************************************
* User interface
******************************************************************************/

grid
as_grid (tree t) {
  array<SI> subd (0, 1);
  array<string> col ("black", "black");
  grid gr= empty_grid ();
  double step= 1.0;
  point center= point (0.0, 0.0);
  if (is_tuple (t, "empty"))
    gr= empty_grid ();
  else
  if (is_tuple (t, "cartesian")) {
    if (is_tuple (t, "cartesian", 0)) ;
    else
    if (is_tuple (t, "cartesian", 1))
      step= as_double (t[1]);
    else
    if (is_tuple (t, "cartesian", 2)) {
      center= as_point (t[1]);
      step= as_double (t[2]);
    }
    gr= cartesian (subd, col, center, step);
  }
  else
  if (is_tuple (t, "polar")) {
    SI astep= 8;
    if (is_tuple (t, "polar", 0)) ;
    else
    if (is_tuple (t, "polar", 1))
      step= as_double (t[1]);
    else
    if (is_tuple (t, "polar", 2)) {
      step= as_double (t[1]);
      astep= as_int (t[2]);
    }
    else
    if (is_tuple (t, "polar", 3)) {
      center= as_point (t[1]);
      step= as_double (t[2]);
      astep= as_int (t[3]);
    }
    gr=polar (subd, col, center, step, astep);
  }
  else
  if (is_tuple (t, "logarithmic")) {
    SI base= 10;
    if (is_tuple (t, "logarithmic", 0)) ;
    else
    if (is_tuple (t, "logarithmic", 1))
      step= as_double (t[1]);
    else
    if (is_tuple (t, "logarithmic", 2)) {
      step= as_double (t[1]);
      base= as_int (t[2]);
    }
    else
    if (is_tuple (t, "logarithmic", 3)) {
      center= as_point (t[1]);
      step= as_double (t[2]);
      base= as_int (t[3]);
    }
    gr= logarithmic (subd, col, center, step, base);
  }
  return gr;
}

tree
as_tree (grid g) {
  return (tree) g;
}
