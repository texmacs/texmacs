
/******************************************************************************
* MODULE     : graphics.cpp
* DESCRIPTION: Boxes for graphics
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "env.hpp"
#include "Boxes/graphics.hpp"
#include "Boxes/composite.hpp"
#include "Graphics/math_util.hpp"

/******************************************************************************
* Graphics boxes
******************************************************************************/

struct graphics_box_rep: public composite_box_rep {
  frame f;
  grid g;
  point lim1, lim2;
  SI old_clip_x1, old_clip_x2, old_clip_y1, old_clip_y2;
  graphics_box_rep (
    path ip, array<box> bs, frame f, grid g, point lim1, point lim2);
  frame get_frame ();
  grid get_grid ();
  void  get_limits (point& lim1, point& lim2);
  operator tree () { return "graphics"; }
  void pre_display (ps_device &dev);
  void post_display (ps_device &dev);
  int reindex (int i, int item, int n);
};

graphics_box_rep::graphics_box_rep (
  path ip2, array<box> bs2, frame f2, grid g2, point lim1b, point lim2b):
  composite_box_rep (ip2, bs2), f (f2), g (g2), lim1 (lim1b), lim2 (lim2b)
{
  point flim1= f(lim1), flim2= f(lim2);
  x1= (SI) min (flim1[0], flim2[0]);
  y1= (SI) min (flim1[1], flim2[1]);
  x2= (SI) max (flim1[0], flim2[0]);
  y2= (SI) max (flim1[1], flim2[1]);
  finalize ();
}

frame
graphics_box_rep::get_frame () {
  return f;
}

grid
graphics_box_rep::get_grid () {
  return g;
}

void
graphics_box_rep::get_limits (point& lim1b, point& lim2b) {
  lim1b= lim1;
  lim2b= lim2;
}

void
graphics_box_rep::pre_display (ps_device &dev) {
  dev->get_clipping (old_clip_x1, old_clip_y1, old_clip_x2, old_clip_y2);
  dev->extra_clipping (x1, y1, x2, y2);
}

void
graphics_box_rep::post_display (ps_device &dev) {
  dev->set_clipping (old_clip_x1, old_clip_y1, old_clip_x2, old_clip_y2);
}

int
graphics_box_rep::reindex (int i, int item, int n) {
  return i;
}

/******************************************************************************
* Point boxes
******************************************************************************/

struct point_box_rep: public box_rep {
  point p;
  SI r;
  color col;
  string style;
  point_box_rep (path ip, point p, SI radius, color col, string style);
  SI graphical_distance (SI x, SI y) { return (SI)norm (p - point (x, y)); }
  void display (ps_device dev);
  operator tree () { return "point"; }
};

point_box_rep::point_box_rep (
  path ip2, point p2, SI r2, color col2, string style2):
    box_rep (ip2), p (p2), r (r2), col (col2), style (style2)
{
  x1= x3= ((SI) p[0]) - r;
  y1= y3= ((SI) p[1]) - r;
  x2= x4= ((SI) p[0]) + r;
  y2= y4= ((SI) p[1]) + r;
}

void
point_box_rep::display (ps_device dev) {
  if (style == "square") {
    dev->set_color (col);
    dev->set_line_style (PIXEL);
    dev->line (((SI) p[0]) - r, ((SI) p[1]) - r,
	       ((SI) p[0]) - r, ((SI) p[1]) + r); 
    dev->line (((SI) p[0]) + r, ((SI) p[1]) + r,
	       ((SI) p[0]) - r, ((SI) p[1]) + r); 
    dev->line (((SI) p[0]) + r, ((SI) p[1]) + r,
	       ((SI) p[0]) + r, ((SI) p[1]) - r); 
    dev->line (((SI) p[0]) - r, ((SI) p[1]) - r,
	       ((SI) p[0]) + r, ((SI) p[1]) - r); 
  }
  else {
  //TODO : Add non filled dots
    int i, n= 4*(r/dev->pixel+1);
    array<SI> x (n), y (n);
    for (i=0; i<n; i++) {
      x[i]= (SI) (p[0] + r * cos ((6.283185307*i)/n));
      y[i]= (SI) (p[1] + r * sin ((6.283185307*i)/n));
    }
    dev->set_color (col);
    dev->polygon (x, y);
  }
}

/******************************************************************************
* Curve boxes
******************************************************************************/

struct curve_box_rep: public box_rep {
  array<point> a;
  SI width;
  color col;
  curve c;
  array<bool> style;
  SI style_unit;
  array<SI> styled_n;
  int fill;
  color fill_col;
  int pattern_mode;
  array<box> pattern;
// Constructor
  void calc_extent (
    SI &X1, SI &Y1, SI &X2, SI &Y2, SI &X3, SI &Y3, SI &X4, SI &Y4);
  curve_box_rep (
    path ip, curve c, SI width, color col, array<bool> style, SI style_unit,
    int fill, color fill_col, int pattern_mode, array<box> pattern);
  operator tree () { return "curve"; }
// Graphical select
  SI graphical_distance (SI x, SI y);
  gr_selections graphical_select (SI x, SI y, SI dist);
// Display
  void curve_box_rep::traverse_subboxes (
    ps_device dev, box b, point p1, point p2, double magn, bool b2, bool draw);
  void display_pattern (ps_device dev, bool draw);
  void display (ps_device dev);
// Operations on translated/rotated curves
  void extent  (point u, double magn, point o, double angle,
		SI &x1, SI &y1, SI &x2, SI &y2,
		SI &x3, SI &y3, SI &x4, SI &y4);
  void display (ps_device dev, point u, double magn, point o, double angle);
// Calculating the style
  SI length ();
  void apply_style ();
};

static ps_device dummy_dev= ps_device ();

// Constructor
void
curve_box_rep::calc_extent (
  SI &X1, SI &Y1, SI &X2, SI &Y2, SI &X3, SI &Y3, SI &X4, SI &Y4)
{
  int i, n= N(a);
  X1= Y1= X3= Y3= MAX_SI;
  X2= Y2= X4= Y4= -MAX_SI;
  for (i=0; i<(n-1); i++) {
    X1= min (X1, min ((SI) a[i][0], (SI) a[i+1][0]));
    Y1= min (Y1, min ((SI) a[i][1], (SI) a[i+1][1]));
    X2= max (X2, max ((SI) a[i][0], (SI) a[i+1][0]));
    Y2= max (Y2, max ((SI) a[i][1], (SI) a[i+1][1]));
  }
  X3= X1 - (width>>1); Y3= Y1 - (width>>1); 
  X4= X2 + (width>>1); Y4= Y2 + (width>>1);
}

static void
calc_composite_extent (box b) {
  b->x1= b->y1= b->x3= b->y3= MAX_SI;
  b->x2= b->y2= b->x4= b->y4= -MAX_SI;
  int i;
  for (i= 0; i<b->subnr(); i++) {
    box sb= b->subbox (i);
    if ((tree)sb == "curve") {
      b->x1= min (b->x1, sb->x1);
      b->y1= min (b->y1, sb->y1);
      b->x2= max (b->x2, sb->x2);
      b->y2= max (b->y2, sb->y2);
      b->x3= min (b->x3, sb->x3);
      b->y3= min (b->y3, sb->y3);
      b->x4= max (b->x4, sb->x4);
      b->y4= max (b->y4, sb->y4);
    }
  }
}

curve_box_rep::curve_box_rep (
  path ip2, curve c2, SI W, color C, array<bool> style2, SI style_unit2,
  int fill2, color fill_col2, int pattern_mode2, array<box> pattern2)
  :
  box_rep (ip2), width (W), col (C), c (c2),
  style (style2), style_unit (style_unit2),
  fill (fill2), fill_col (fill_col2),
  pattern_mode (pattern_mode2), pattern (pattern2)
{
  a= c->rectify (PIXEL);
  calc_extent (x1, y1, x2, y2, x3, y3, x4, y4);
  if (N(pattern)>0 && !nil (pattern[0]))
    calc_composite_extent (pattern[0]);
  if (N(pattern)>1 && !nil (pattern[1]))
    calc_composite_extent (pattern[1]);
  if (N(pattern)>2 && !nil (pattern[2]))
    calc_composite_extent (pattern[2]);
  display_pattern (dummy_dev, false);
  apply_style ();
}

// Graphical select
SI
curve_box_rep::graphical_distance (SI x, SI y) {
  SI gd= MAX_SI;
  point p (x, y);
  int i;
  for (i=0; i<N(a)-1; i++) {
    axis ax;
    ax.p0= a[i];
    ax.p1= a[i+1];
    gd= min (gd, (SI)seg_dist (ax, p));
  }
  return gd;
}

gr_selections
curve_box_rep::graphical_select (SI x, SI y, SI dist) {
  gr_selections res;
  if (graphical_distance (x, y) <= dist) {
    array<double> abs;
    array<point> pts;
    array<path> paths;
    int np= c->get_control_points (abs, pts, paths);
    point p (x, y);
    int i;
    for (i=0; i<N(pts); i++) {
      SI n= (SI)norm (p - pts[i]);
      if (n <= dist) {
        gr_selection gs;
        gs->dist= n;
        gs->cp << reverse (paths[i]);
        res << gs;
      }
    }
    int ne= np-1;
    if (np>1 && (abs[0]!=0.0 || abs[np-1]!=1.0))
      ne++;
    for (i=0; i<ne; i++) {
      bool b;
      double t= c->find_closest_point (abs[i], abs[(i+1)%np], p, PIXEL, b);
      if (b) {
        point p2= c->evaluate (t);
        SI n= (SI)norm (p - p2);
        if (n <= dist) {
          gr_selection gs;
          gs->dist= n;
          gs->cp << reverse (paths[i]);
          gs->cp << reverse (paths[(i+1)%np]);
          res << gs;
        }
      }
    }
  }
  return res;
}

// Display
static bool
find_first_point (array<point> a, point &p) {
  p= point ();
  if (N(a)<=0) return false;
  p= a[0];
  return true;
}

static bool
find_last_point (array<point> a, point &p) {
  p= point ();
  if (N(a)<=0) return false;
  p= a[N(a)-1];
  return true;
}

static bool
find_prev_last (array<point> a, point &p) {
  p= point ();
  if (N(a)<=0) return false;
  int i= N(a)-1;
  point p0= a[i];
  while (i>=0) {
    if (!fnull (norm(a[i]-p0),1e-6)) {
      p= a[i];
      return true;
    }
    i--;
  }
  return false;
}

static bool
find_next_first (array<point> a, point &p) {
  p= point ();
  if (N(a)<=0) return false;
  int i= 0;
  point p0= a[i];
  while (i<N(a)) {
    if (!fnull (norm(a[i]-p0),1e-6)) {
      p= a[i];
      return true;
    }
    i++;
  }
  return false;
}

void
curve_box_rep::traverse_subboxes (
  ps_device dev, box b, point p1, point p2, double magn, bool b2, bool draw)
{
  if (!nil (b)) {
    point o1 (2), o2 (2);
    o1[0]= b2 ? b->x2 : b->x1;
    o1[1]= (b->y1 + b->y2) / 2;
    o2= b2 ? p2 : p1;
    double angle= arg (p2-p1);
    int i;
    for (i= 0; i<b->subnr(); i++) {
      box sb= b->subbox (i);
      if (draw)
        sb->display (dev, point (sb->x1, sb->y1) +o2 -o1, magn, o2, angle);
      else {
        if ((tree)sb == "curve") {
          SI X1, Y1, X2, Y2;
          SI X3, Y3, X4, Y4;
          sb->extent (point (sb->x1, sb->y1) +o2 -o1, magn, o2, angle,
		      X1, Y1, X2, Y2, X3, Y3, X4, Y4);
          x1= min (x1, X1);
          y1= min (y1, Y1);
          x2= max (x2, X2);
          y2= max (y2, Y2);
          x3= min (x3, X3);
          y3= min (y3, Y3);
          x4= max (x4, X4);
          y4= max (y4, Y4);
        }
      }
    }
  }
}

void
curve_box_rep::display_pattern (ps_device dev, bool draw) {
  int n= N(a);
  if (n<=1) return;
  SI l0= 0, l1=0;
  point p1, p2;
  if (N(pattern)>0 && find_first_point (a, p1) && find_next_first (a, p2)) {
    traverse_subboxes (dev, pattern[0], p1, p2, 1.0, false, draw);
    if (!nil (pattern[0])) l0= pattern[0]->x2 - pattern[0]->x1;
  }
  if (N(pattern)>1 && find_prev_last (a, p1) && find_last_point (a, p2)) {
    traverse_subboxes (dev, pattern[1], p1, p2, 1.0, true, draw);
    if (!nil (pattern[1])) l1= pattern[1]->x2 - pattern[1]->x1;
  }
  if (N(pattern)>2) {
    box rep= pattern[2];

    if (!nil (rep)) {
      SI L= length (), L2= L -l0 -l1, lm= rep->x2 - rep->x1;
      if (L2<lm) return; //FIXME: Should shrink the pattern
      SI l= L2 / (L2 / lm);
      SI li= 0, li1= (SI)norm (a[1]-a[0]), len= l0;
    //FIXME: Thats better if we start from l0+l/2, drawing the
    //  pattern accordingly (i.e., using the middle point on
    //  the curve in the area where the pattern is displayed)

      int i= 0;
      while (len<=L-l1-l+1e-6 && i+1<n) {
        point seg= a[i+1]-a[i];
        while (fnull (norm(seg),1e-6) && i+2<n) {
          i++;
          li= li1;
          seg= a[i+1]-a[i];
          li1+= (SI)norm (seg);
        }
        if (fnull (norm(seg),1e-6) && i+2>=n)
          break;
        while (li1<len && i+1<n) {
          i++;
          if (i+1<n) {
            li= li1;
            seg= a[i+1]-a[i];
            li1+= (SI)norm (seg);
          }
        }
        if (i+1>=n) break;
        point o= a[i] + (len-li) * (seg/norm(seg));
        traverse_subboxes (dev, rep,
	  o, a[i+1], ((double)l)/((double)lm), false, draw); 
        len+= l;
      }
    }
  }
}

void
curve_box_rep::display (ps_device dev) {
  int i, n;
  if (fill == FILL_MODE_INSIDE || fill == FILL_MODE_BOTH) {
    dev->set_color (fill_col);
    n= N(a);
    array<SI> x (n), y (n);
    for (i=0; i<n; i++) {
      x[i]= (SI)a[i][0];
      y[i]= (SI)a[i][1];
    }
    dev->polygon (x, y, false);
  }
  if ((fill == FILL_MODE_NONE || fill == FILL_MODE_BOTH) &&
      pattern_mode != LINE_PATTERN_MODE_PATTERN)
  {
    dev->set_color (col);
    dev->set_line_style (width, 0, false);
    if (N (style) == 0) {
      n= N(a);
      for (i=0; i<(n-1); i++)
        dev->line ((SI) a[i][0], (SI) a[i][1], (SI) a[i+1][0], (SI) a[i+1][1]);
    }
    else {
      SI li=0, o=0;
      i=0;
      int no;
      point prec;
      for (no=0; no<N(styled_n); no++) {
        point seg= a[i+1]-a[i];
        while (fnull (norm(seg),1e-6) && i+2<N(a)) {
          i++;
          seg= a[i+1]-a[i];
        }
        if (fnull (norm(seg),1e-6) && i+2>=N(a))
          break;
        SI lno= styled_n[no]*style_unit,
           len= li+(SI)norm(seg);
        while (lno>len) {
          li= len;
          if (no%2!=0) {
            dev->line ((SI) prec[0], (SI) prec[1],
                       (SI) a[i+1][0], (SI) a[i+1][1]);
  	    prec= a[i+1];
          }
          i++;
          seg= a[i+1]-a[i];
          len= li+(SI)norm(seg);
        }
        o= lno-li;
     /* We could also use this one in order to use lines with
        round ends. But it doesn't work well when the width
        of the line becomes bigger than the style unit length.
        Anyway (although I don't know if there is a way to do
        lines with round ends in PostScript), our current PostScript
        output in GhostView uses square line ends, so we do the same.
        SI inc= ((no%2==0?1:-1) * width)/2;
        point b= a[i] + (o+inc)*(seg/norm(seg)); */
        point b= a[i] + o*(seg/norm(seg));
        if (no%2==0)
          prec= b;
        else
          dev->line ((SI) prec[0], (SI) prec[1], (SI) b[0], (SI) b[1]);
      }
    }
  }
  if ((fill == FILL_MODE_NONE || fill == FILL_MODE_BOTH) &&
      (pattern_mode == LINE_PATTERN_MODE_PATTERN ||
       pattern_mode == LINE_PATTERN_MODE_BOTH
    ))
    display_pattern (dev, true);
}

// Operations on translated/rotated curves
void
curve_box_rep::extent (point u, double magn, point o, double angle,
  SI &X1, SI &Y1, SI &X2, SI &Y2, SI &X3, SI &Y3, SI &X4, SI &Y4)
{
  array<point> a0= a;
  a= array<point> (N(a));
  int i;
  for (i=0; i<N(a); i++) {
    a[i]= a0[i] -point (x1, y1);
    a[i]= magn * a[i];
    a[i]= a[i] + u;
    a[i]= rotate2D (a[i], o, angle);
  }
  calc_extent (X1, Y1, X2, Y2, X3, Y3, X4, Y4);
  a= a0;
}

void
curve_box_rep::display (
  ps_device dev, point u, double magn, point o, double angle)
{
  array<point> a0= a;
  a= array<point> (N(a));
  int i;
  for (i=0; i<N(a); i++) {
    a[i]= a0[i] -point (x1, y1);
    a[i]= magn * a[i];
    a[i]= a[i] + u;
    a[i]= rotate2D (a[i], o, angle);
  }
  display (dev);
  a= a0;
}

// Calculating the style
SI
curve_box_rep::length () {
  int i, n= N(a);
  SI res= 0;
  for (i=1; i<n; i++)
    res+= (SI)norm (a[i] - a[i-1]);
  return res;
}

void
curve_box_rep::apply_style () {
  int n= N(style);
  if (n<=0 || fnull (style_unit,1e-6)) return;
  SI l= length (), l1= n*style_unit, n1= l/l1 + 1;
  l1= l/n1;
  style_unit= l1/n;

  int i, nfrag=0, prevfrag=-1;
  bool all0=true, all1=true;
  for (i=0; i<n; i++) {
    int frag= style[i]?1:0;
    if (frag!=prevfrag && frag==1) nfrag++;
    if (style[i]) all0= false;
    if (!style[i]) all1= false;
    prevfrag= frag;
  }
  if (all1) style= array<bool>(0);
  if (all0 || all1) return;

  bool common_frag= style[0] && style[n-1] && n1>1;
  if (common_frag) nfrag--;
  styled_n= array<SI>(2*nfrag*n1 + (common_frag?2:0));

  int no=0, nbu=0;
  prevfrag=-1;
  for (i=0; i<n1; i++) {
    int j;
    for (j=0; j<n; j++) {
      int frag= style[j]?1:0;
      if (frag!=prevfrag) {
        if (frag==1) {
          styled_n[no]= nbu;
          no++;
        }
        else
        if (frag==0 && prevfrag!=-1) {
          styled_n[no]= nbu;
          no++;
        }
      }
      prevfrag= frag;
      nbu++;
    }
  }
  if (style[n-1]) styled_n[N(styled_n)-1]= nbu;
}

/******************************************************************************
* User interface
******************************************************************************/

box
graphics_box (
  path ip, array<box> bs, frame f, grid g, point lim1, point lim2)
{
  return new graphics_box_rep (ip, bs, f, g, lim1, lim2);
}

box
point_box (path ip, point p, SI r, color col, string style) {
  return new point_box_rep (ip, p, r, col, style);
}

box
curve_box (
  path ip, curve c, SI width, color col, array<bool> style, SI style_unit,
  int fill, color fill_col, int pattern_mode, array<box> pattern)
{
  return new curve_box_rep (ip, c, width, col, style, style_unit,
			    fill, fill_col, pattern_mode, pattern);
}
