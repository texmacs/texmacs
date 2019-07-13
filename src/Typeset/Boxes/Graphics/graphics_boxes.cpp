
/******************************************************************************
* MODULE     : graphics.cpp
* DESCRIPTION: Boxes for graphics
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "env.hpp"
#include "Boxes/graphics.hpp"
#include "Boxes/composite.hpp"
#include "Boxes/construct.hpp"
#include "math_util.hpp"

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
  void pre_display (renderer &ren);
  void post_display (renderer &ren);
  int reindex (int i, int item, int n);
  virtual int find_child (SI x, SI y, SI delta, bool force);
  gr_selections graphical_select (SI x, SI y, SI dist);
  gr_selections graphical_select (SI x1, SI y1, SI x2, SI y2);
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
  x3= max (x1, x3);
  y3= max (y1, y3);
  x4= min (x2, x4);
  y4= min (y2, y4);
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
graphics_box_rep::pre_display (renderer &ren) {
  ren->get_clipping (old_clip_x1, old_clip_y1, old_clip_x2, old_clip_y2);
  ren->extra_clipping (x1, y1, x2, y2);
}

void
graphics_box_rep::post_display (renderer &ren) {
  ren->set_clipping (
    old_clip_x1, old_clip_y1, old_clip_x2, old_clip_y2, true);
}

int
graphics_box_rep::reindex (int i, int item, int n) {
  (void) item; (void) n;
  return i;
}

int
graphics_box_rep::find_child (SI x, SI y, SI delta, bool force) {
  int m= composite_box_rep::find_child (x, y, delta, force);
  if (m==-1) return -1;
  int i, n= subnr();
  for (i=0; i<n; i++)
    if (distance (i, x, y, delta)==0) {
      tree ty= (tree)bs[i];
      if ((bs[i]->accessible () || force) && is_tuple (ty) && ty[0]=="text-at")
        return i;
    }
  return m;
}

/*NOTE: It seems that the dimensions of the boxes that inherit from
  composite_box are not calculated correctly (namely : one can find
  points inside the box that are outside the rectangle (x1, y1, x2, y2)
  that defines the border of the box). As a consequence, we use a traversal
  routine that doesn't tests contains_rectangle(). When this problem
  will have been corrected, the method of composite_box should work,
  and consequently, its more specific implementation below should be
  removed (this is the same in concat_boxes and stack_boxes). */

gr_selections
graphics_box_rep::graphical_select (SI x, SI y, SI dist) {
  gr_selections res;
  int i, n= subnr();
  for (i=n-1; i>=0; i--)
    res << bs[i]->graphical_select (x- sx(i), y- sy(i), dist);
  return res;
}

gr_selections
graphics_box_rep::graphical_select (SI x1, SI y1, SI x2, SI y2) {
  gr_selections res;
  int i, n= subnr();
  for (i=n-1; i>=0; i--)
    res << bs[i]->graphical_select (x1- sx(i), y1- sy(i),
				    x2- sx(i), y2- sy(i));
  return res;
}

/******************************************************************************
* Group boxes
******************************************************************************/

struct graphics_group_box_rep: public composite_box_rep {
  graphics_group_box_rep (path ip, array<box> bs):
    composite_box_rep (ip, bs, true) { finalize (); }
  bool access_allowed () { return false; }
  operator tree () { return "graphics_group"; }
  path find_lip () { return path (-1); }
  path find_rip () { return path (-1); }
  gr_selections graphical_select (SI x, SI y, SI dist);
  gr_selections graphical_select (SI x1, SI y1, SI x2, SI y2);
  int reindex (int i, int item, int n);
};

gr_selections
graphics_group_box_rep::graphical_select (SI x, SI y, SI dist) {
  gr_selections res;
  if (graphical_distance (x, y) <= dist) {
    gr_selection gs;
    gs->type= "group";
    gs->dist= graphical_distance (x, y);
  //gs->p= point (x, y); // The cursor moves freely inside the box
    gs->cp << reverse (path (0, ip));
    gs->pts= array<point> (0);
    gs->c= curve ();
    res << gs;
  }
  return res;
}

gr_selections
graphics_group_box_rep::graphical_select (SI x1, SI y1, SI x2, SI y2) {
  gr_selections res;
  if (in_rectangle (x1, y1, x2, y2)) {
    gr_selection gs;
    gs->type= "group";
    gs->dist= graphical_distance (x1, y1);
    gs->cp << reverse (path (0, ip));
    gs->pts= array<point> (0);
    gs->c= curve ();
    res << gs;
  }
  return res;
}

int
graphics_group_box_rep::reindex (int i, int item, int n) {
  (void) item; (void) n;
  return i;
}

/******************************************************************************
* Point boxes
******************************************************************************/

struct point_box_rep: public box_rep {
  point p;
  SI r;
  pencil pen;
  brush br;
  string style;
  point_box_rep (path ip, point p, SI radius, pencil pen,
		 brush br, string style);
  SI graphical_distance (SI x, SI y) { return (SI) norm (p - point (x, y)); }
  gr_selections graphical_select (SI x, SI y, SI dist);
  void display (renderer ren);
  operator tree () { return "point"; }
};

point_box_rep::point_box_rep (path ip2, point p2, SI r2, pencil pen2,
			      brush br2, string style2):
    box_rep (ip2), p (p2), r (r2), pen (pen2),
    br (br2), style (style2)
{
  SI w= pen->get_width () >> 1;
  x1= x3= ((SI) p[0]) - r - w;
  y1= y3= ((SI) p[1]) - r - w;
  x2= x4= ((SI) p[0]) + r + w;
  y2= y4= ((SI) p[1]) + r + w;
}

static array<point>
get_contour (string style) {
  array<point> a;
  if (style == "square")
    a << point (-1.0, -1.0) << point ( 1.0, -1.0)
      << point ( 1.0,  1.0) << point (-1.0,  1.0);
  else if (style == "diamond")
    a << point (-1.0,  0.0) << point ( 0.0, -1.0)
      << point ( 1.0,  0.0) << point ( 0.0,  1.0);
  else if (style == "triangle")
    a << point (-1.0, -1.0) << point ( 1.0, -1.0)
      << point ( 0.0,  1.0);
  else if (style == "star") {
    int n= 5;
    for (int i=0; i<2*n; i++) {
      double b= (2.0 * 3.141592653) * (1.0 * i) / (2.0 * n);
      double r= ((i&1) == 0? 1.0: 0.5);
      double x= r * sin (b);
      double y= r * cos (b);
      a << point (x, y);
    }
  }
  else if (style == "plus")
    a << point ( 0.0,  0.0) << point ( 1.0,  0.0)
      << point ( 0.0,  0.0) << point ( 0.0,  1.0)
      << point ( 0.0,  0.0) << point (-1.0,  0.0)
      << point ( 0.0,  0.0) << point ( 0.0, -1.0);
  else if (style == "cross")
    a << point ( 0.0,  0.0) << point ( 1.0,  1.0)
      << point ( 0.0,  0.0) << point (-1.0,  1.0)
      << point ( 0.0,  0.0) << point (-1.0, -1.0)
      << point ( 0.0,  0.0) << point ( 1.0, -1.0);
  return a;
}

gr_selections
point_box_rep::graphical_select (SI x, SI y, SI dist) {
  gr_selections res;
  if (graphical_distance (x, y) <= dist) {
    gr_selection gs;
    gs->type= "point";
    gs->dist= graphical_distance (x, y);
    gs->p= p;
    gs->cp << reverse (path (0, ip));
    gs->pts << p;
    gs->c= curve ();
    res << gs;
  }
  return res;
}

void
point_box_rep::display (renderer ren) {
  array<point> a= get_contour (style);
  for (int i=0; i<N(a); i++)
    a[i]= p + ((double) r) * a[i];
  if (style == "none");
  else if (N(a) != 0) {
    if (br->get_type () != brush_none) {
      ren->set_pencil (pen);
      ren->set_brush (br);
      for (int i=0; i<N(a); i++) {
	int j= (i+1) % (N(a));
	ren->line ((SI) a[i][0], (SI) a[i][1], (SI) a[j][0], (SI) a[j][1]);
      }
      array<SI> x (N(a)), y (N(a));
      for (int i=0; i<N(a); i++) {
	x[i]= (SI) a[i][0];
	y[i]= (SI) a[i][1];
      }
      ren->polygon (x, y, false);
    }
    if (pen->get_type () != pencil_none) {
      ren->set_pencil (pen);
      for (int i=0; i<N(a); i++) {
	int j= (i+1) % (N(a));
	ren->line ((SI) a[i][0], (SI) a[i][1], (SI) a[j][0], (SI) a[j][1]);
      }
    }
  }
  else {
    SI w = (SI) pen->get_width ();
    SI cx= (SI) p[0];
    SI cy= (SI) p[1];
    SI lx= cx - r;
    SI by= cy - r;
    SI rx= cx + r;
    SI ty= cy + r;
    if (style == "disk" || br->get_type () != brush_none) {
      ren->set_brush (style == "disk" ? pen->get_brush () : br);
      ren->arc (lx, by+w, rx, ty+w, 0, 64*360);
      ren->fill_arc (lx, by+w, rx, ty+w, 0, 64*360);
    }
    if (pen->get_type () != pencil_none) {
      ren->set_pencil (pen);
      ren->arc (lx, by+w, rx, ty+w, 0, 64*360);
    }
  }
}

/******************************************************************************
* Curve boxes
******************************************************************************/

struct curve_box_rep: public box_rep {
  array<point> a;
  pencil pen;
  curve c;
  array<bool> style;
  array<point> motif;
  SI style_unit;
  array<SI> styled_n;
  brush fill_br;
  array<box> arrows;
  curve_box_rep (path ip, curve c, pencil pen,
		 array<bool> style, array<point> motif, SI style_unit,
		 brush fill_br,
		 array<box> arrows);
  box transform (frame fr);
  SI graphical_distance (SI x, SI y);
  gr_selections graphical_select (SI x, SI y, SI dist);
  gr_selections graphical_select (SI x1, SI y1, SI x2, SI y2);
  void display (renderer ren);
  operator tree () { return "curve"; }
  SI length ();
  void apply_style ();
  void apply_motif (array<box> arrows);
};

curve_box_rep::curve_box_rep (path ip2, curve c2, pencil pen2,
  array<bool> style2, array<point> motif2, SI style_unit2,
  brush fill_br2, array<box> arrows2):
    box_rep (ip2), pen (pen2), c (c2),
    style (style2), motif (motif2), style_unit (style_unit2),
    fill_br (fill_br2)
{
  a= c->rectify (PIXEL / 4);
  apply_motif (arrows2);
  int i, n= N(a);
  x1= y1= x3= y3= MAX_SI;
  x2= y2= x4= y4= -MAX_SI;
  for (i=0; i<(n-1); i++) {
    x1= min (x1, min ((SI) a[i][0], (SI) a[i+1][0]));
    y1= min (y1, min ((SI) a[i][1], (SI) a[i+1][1]));
    x2= max (x2, max ((SI) a[i][0], (SI) a[i+1][0]));
    y2= max (y2, max ((SI) a[i][1], (SI) a[i+1][1]));
  }
  apply_style ();
  arrows= array<box>(2);
  point p1, p2;
  bool error;
  if (N(arrows2)>0 && !is_nil (arrows2[0])) {
    point tg= c->grad (0.0, error);
    if (!error) {
      frame fr= scaling (1.0, a[0]) *
		rotation_2D (point (0.0, 0.0), arg (tg));
      arrows[0]= arrows2[0]->transform (fr);
      if (!is_nil (arrows[0])) {
	x1= min (x1, arrows[0]->x1);
	y1= min (y1, arrows[0]->y1);
	x2= max (x2, arrows[0]->x2);
	y2= max (y2, arrows[0]->y2);
      }
    }
  }
  if (N(arrows2)>1 && !is_nil (arrows2[1])) {
    point tg= c->grad (1.0, error);
    if (!error) {
      frame fr= scaling (1.0, a[N(a)-1]) *
		rotation_2D (point (0.0, 0.0), arg (tg));
      arrows[1]= arrows2[1]->transform (fr);
      if (!is_nil (arrows[1])) {
	x1= min (x1, arrows[1]->x1);
	y1= min (y1, arrows[1]->y1);
	x2= max (x2, arrows[1]->x2);
	y2= max (y2, arrows[1]->y2);
      }
    }
  }
  SI width= pen->get_width ();
  x3= x1 - (width>>1); y3= y1 - (width>>1); 
  x4= x2 + (width>>1); y4= y2 + (width>>1);
}

box
curve_box_rep::transform (frame fr) {
  return curve_box (ip, fr (c), 1.0, pen, style, motif, style_unit,
                    fill_br, arrows);
}

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
  array<double> abs;
  array<point> pts;
  array<path> paths;
  int np= c->get_control_points (abs, pts, paths);
  for (i=0; i<np; i++)
    gd= min (gd, (SI) norm (pts[i] - p));
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
	gs->type= "curve-handle";
	gs->dist= n;
	gs->p= pts[i];
	gs->cp << reverse (paths[i]);
	gs->pts << pts[i];
	gs->c= c;
	res << gs;
      }
    }
    if (N(res) != 0) return res;
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
	  gs->type= "curve-point";
	  gs->dist= n;
	  gs->p= p2;
	  gs->cp << reverse (paths[i]);
	  gs->cp << reverse (paths[(i+1)%np]);
	  gs->pts << pts[i];
	  gs->pts << pts[(i+1)%np];
	  gs->c= c;
	  res << gs;
	}
      }
    }
  }
  return res;
}

gr_selections
curve_box_rep::graphical_select (SI x1, SI y1, SI x2, SI y2) {
  gr_selections res;
  if (in_rectangle (x1, y1, x2, y2)) {
    gr_selection gs;
    gs->type= "curve";
    gs->dist= graphical_distance (x1, y1);
    gs->cp << reverse (path (0, ip));
    gs->pts= array<point> (0);
    gs->c= c;
    res << gs;
  }
  return res;
}

void
curve_box_rep::display (renderer ren) {
  int i, n;
  if (fill_br->get_type () != brush_none) {
    ren->set_brush (fill_br);
    n= N(a);
    array<SI> x (n), y (n);
    for (i=0; i<n; i++) {
      x[i]= (SI)a[i][0];
      y[i]= (SI)a[i][1];
    }
    ren->polygon (x, y, false);
  }
  if (pen->get_type () != pencil_none) {
    ren->set_pencil (pen->set_cap (cap_flat));
 // TODO: Add options for handling round/nonround joins & line ends
    if (N (style) == 0) {
      n= N(a);
      array<SI> x (n), y (n);
      for (i=0; i<n; i++) {
	x[i]= (SI) a[i][0];
	y[i]= (SI) a[i][1];
      }
      ren->lines (x, y);
    }
    else {
      SI li=0, o=0;
      i=0;
      int no;
      point prec= a[0];
      for (no=0; no<N(styled_n); no++) {
	array<SI> x, y;
	point seg= a[i+1]-a[i];
	while (fnull (norm(seg),1e-6) && i+2<N(a)) {
	  i++;
	  seg= a[i+1]-a[i];
	}
	if (fnull (norm(seg),1e-6) && i+2>=N(a))
	  break;
	SI lno= styled_n[no]*style_unit,
	   len= li+(SI)norm(seg);
	while (i+2<N(a) && lno>len) {
	  li= len;
	  if (no%2!=0) {
         // 1st subsegment of a dash, along with the next ones
	    x << (SI) prec[0];
	    y << (SI) prec[1];
	    prec= a[i+1];
	  }
	  i++;
	  seg= a[i+1]-a[i];
	  len= li+(SI)norm(seg);
	}
	if (N(x)>0 && no%2!=0) {
	  x << (SI) prec[0];
	  y << (SI) prec[1];
	}
	o= lno-li;
	if (i<N(a)) {
	  point b= a[i] + o*(seg/norm(seg));
	  if (no%2==0)
	    prec= b;
	  else {
	 // Last subsegment of a dash
	    if (N(x)==0) {
	      x << (SI) prec[0];
	      y << (SI) prec[1];
	    }
	    x << (SI) b[0];
	    y << (SI) b[1];
	  }
	}
	ren->lines (x, y);
      }
    }
  }

  rectangles ll;
  if (!is_nil (arrows[0])) arrows[0]->redraw (ren, path (), ll);
  if (!is_nil (arrows[1])) arrows[1]->redraw (ren, path (), ll);
}

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
  int i;
  bool all0=true, all1=true;
  for (i=0; i<n; i++) {
    if (style[i]) all0= false;
    if (!style[i]) all1= false;
  }
  if (all1) style= array<bool>(0);
  if (all0 || all1) return;

  int n2= 0;
  i= 0;
  while (!style[i]) i++;
  while (i<N(style)) {
    if (style[i]) n2++; else break;
    i++;
  }

  if (a[0]==a[N(a)-1]) n2= n; // Closed curves

  SI l= length (), l1= n*style_unit, n1= l/l1 + 1, l2= n2*style_unit;
  l1= (SI)((((double)l)*((double)l1)) / ((double)(n1*l1 + l2)));
  style_unit= l1/n;
  l2= n2*style_unit;

  int nfrag=0, prevfrag=-1;
  for (i=0; i<n; i++) {
    int frag= style[i]?1:0;
    if (frag!=prevfrag && frag==1) nfrag++;
    prevfrag= frag;
  }

  int nfrag2=0;
  prevfrag=-1;
  for (i=0; i<n2; i++) {
    int frag= style[i]?1:0;
    if (frag!=prevfrag && frag==1) nfrag2++;
    prevfrag= frag;
  }

  bool common_frag= style[0] && style[n-1] && n1>1;
  if (common_frag) nfrag--;
  styled_n= array<SI>(2*(nfrag*n1 + nfrag2));

  int no=0, nbu=0;
  prevfrag=-1;
  for (i=0; i<n1+1; i++) {
    int j;
    for (j=0; j<(i==n1?n2:n); j++) {
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
  if (style[n2-1]) styled_n[N(styled_n)-1]= nbu;
}

void
curve_box_rep::apply_motif (array<box> arrows) {
  int n= N(motif);
  if (n <= 0 || fnull (style_unit,1e-6)) return;
  SI l= length ();
  int k= l / style_unit;
  if (k<=0) return;
  SI unit= l / k;
  array<point> b;
  b << a[0];
  SI rem= 0;
  for (int i=1; i<N(a); i++) {
    double no= norm (a[i] - a[i-1]);
    double t= 0.0;
    while (true) {
      SI res= (SI) ((1.0 - t) * no);
      if (rem + res < unit - 32) {
        // NOTE: 32 serves as safety margin
        rem += res;
        break;
      }
      t += ((double) (unit - rem)) / no;
      if (t >= 0.999) t= 1.0;
      point d= t * (a[i] - a[i-1]);
      b << (a[i-1] + d);
      rem= 0;
    }
  }
  if (N(b) <= 2 && b[0] == b[N(b)-1]) return;
  a= b;
  b= array<point> ();
  for (int i=0; i<N(a); i++) {
    point cur = a[i];
    point prev= (i == 0? a[N(a)-2]: a[i-1]);
    point next= (i == N(a)-1? a[1]: a[i+1]);
    if (i == 0 && a[0] != a[N(a)-1])
      prev= cur - (next - cur);
    if (i == N(a) - 1 && a[0] != a[N(a)-1])
      next= cur + (cur - prev);
    point u= (next == prev? point (0.0, 0.0):
              unit * (next - prev) / norm (next - prev));
    b << (cur + point (-u[1],  u[0]));
  }
  array<point> ta;
  ta << a[0];
  for (int i=0; i+1<N(a); i++) {
    array<point> mot= motif;
    if ((i == 0 && N(arrows) > 0 && !is_nil (arrows[0])) ||
        (i == N(a)-2 && N(arrows) > 1 && !is_nil (arrows[1]))) {
      mot= array<point> (0);
      mot << point (0.0, 0.0);
      mot << point (1.0, 0.0);
    }
    for (int j=0; j<N(mot); j++) {
      double x= mot[j][0];
      double y= mot[j][1];
      point u= a[i] + x * (a[i+1] - a[i]);
      point v= b[i] + x * (b[i+1] - b[i]);
      point w= u + y * (v - u);
      if (w != ta[N(ta)-1]) ta << w;
    }
  }
  ta << a[N(a)-1];
  a= ta;
}

/******************************************************************************
* 3D graphics
******************************************************************************/

struct spacial_box_rep: public box_rep {
  spacial obj;
  spacial_box_rep (path ip, spacial obj2);
  void display (renderer ren);
  operator tree () { return "spacial"; }
};

spacial_box_rep::spacial_box_rep (path ip, spacial obj2):
  box_rep (ip), obj (obj2)
{
  rectangle r= obj->get_extents ();
  x1= x3= r->x1;
  y1= y3= r->y1;
  x2= x4= r->x2;
  y2= y4= r->y2;
}

void
spacial_box_rep::display (renderer ren) {
  ren->draw_spacial (obj);
}

/******************************************************************************
* User interface
******************************************************************************/

box
graphics_box (
  path ip, array<box> bs, frame f, grid g, point lim1, point lim2)
{
  box r= tm_new<graphics_box_rep> (ip, bs, f, g, lim1, lim2);
  if (r->x1 != 0) r= move_box (ip, r, -r->x1, 0);
  return r;
}

box
graphics_group_box (path ip, array<box> bs) {
  return tm_new<graphics_group_box_rep> (ip, bs);
}

box
point_box (path ip, point p, SI r, pencil pen, brush br, string style) {
  return tm_new<point_box_rep> (ip, p, r, pen, br, style);
}

box
curve_box (path ip, curve c, double portion, pencil pen,
           array<bool> style, array<point> motif, SI style_unit,
           brush fill_br, array<box> arrows)
{
  if (portion < 1.0) c= truncate (c, max (portion, 1.0e-5), PIXEL / 10.0);
  return tm_new<curve_box_rep> (ip, c, pen, style, motif, style_unit,
                                fill_br, arrows);
}

box
spacial_box (path ip, spacial obj) {
  return tm_new<spacial_box_rep> (ip, obj);
}
