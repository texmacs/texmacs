
/******************************************************************************
* MODULE     : raster.hpp
* DESCRIPTION: Raster pictures
* COPYRIGHT  : (C) 2013  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#ifndef RASTER_H
#define RASTER_H
#include "raster_operators.hpp"
#include "unary_function.hpp"

/******************************************************************************
* Raster class
******************************************************************************/

template<typename C> class raster;
template<typename C> bool is_nil (raster<C> r);

template<typename C>
class raster_rep: public abstract_struct {
public:
  int w, h;
  int ox, oy;
  C* a;

public:
  raster_rep (int w2, int h2, int ox2, int oy2):
    w (w2), h (h2), ox (ox2), oy (oy2), a (NULL) {
      if (w * h != 0) a= tm_new_array<C> (w * h); }
  ~raster_rep () { if (w * h != 0) tm_delete_array (a); }

  C internal_get_pixel (int x, int y) {
    //cout << "Normal " << x << ", " << y << "\n";
    if (x >= 0 && w > x && y >= 0 && h > y) return a[y*w+x];
    else { C r; clear (r); return r; } }
  C internal_smooth_pixel (double x, double y) {
    //cout << "Smooth " << x << ", " << y << "\n";
    x -= 0.5; y -= 0.5;
    int x1= (int) floor (x);
    int y1= (int) floor (y);
    int x2= x1 + 1;
    int y2= y1 + 1;
    double ix1= x2 - x;
    double ix2= x - x1;
    double iy1= y2 - y;
    double iy2= y - y1;
    C cx1y1= internal_get_pixel (x1, y1);
    C cx1y2= internal_get_pixel (x1, y2);
    C cx2y1= internal_get_pixel (x2, y1);
    C cx2y2= internal_get_pixel (x2, y2);
    //return mix (mix (cx1y1, ix1, cx2y1, ix2), iy1,
    //mix (cx1y2, ix1, cx2y2, ix2), iy2);
    return mix (cx1y1, ix1*iy1, cx1y2, ix1*iy2,
		cx2y1, ix2*iy1, cx2y2, ix2*iy2); }
  inline C get_pixel (int x, int y) {
    return internal_get_pixel (x + ox, y + oy); }
  inline C smooth_pixel (double x, double y) {
    return internal_smooth_pixel (x + ox, y + oy); }
};

template<typename C> class raster {
  ABSTRACT_NULL_TEMPLATE(raster,C);
  inline raster (int w, int h, int ox, int oy):
    rep (tm_new<raster_rep<C> > (w, h, ox, oy)) { INC_COUNT(rep); }
};
ABSTRACT_NULL_TEMPLATE_CODE(raster,class,C);

template<typename C> void
print (raster<C> r) {
  for (int y=0; y<r->h; y++)
    for (int x=0; x<r->w; x++)
      cout << x << ", " << y << " -> " << r->a[y*r->w+x] << "\n";
}

/******************************************************************************
* Simple operations
******************************************************************************/

template<typename C> raster<C>
subraster (raster<C> r, int x1, int y1, int x2, int y2) {
  x1 += r->ox; y1 += r->oy; x2 += r->ox; y2 += r->oy;
  int w= r->w, h= r->h;
  x1= max (0, x1); y1= max (0, y1);
  x2= min (w, x2); y2= min (h, y2);
  int ww= x2 - x1, hh= y2 - y1;
  raster<C> ret (ww, hh, r->ox - x1, r->oy - y1);
  for (int y=y1; y<y2; y++)
    for (int x=x1; x<x2; x++)
      ret->a[ww*(y-y1) + (x-x1)]= r->a[w*y+x];
  return ret;
}

template<typename C> raster<C>
change_extents (raster<C> r, int w, int h, int ox, int oy) {
  raster<C> ret (w, h, ox, oy);
  for (int y=0; y<h; y++)
    for (int x=0; x<w; x++)
      ret->a[w*y+x]= r->get_pixel (x - ox, y - oy);
  return ret;
}

template<typename C> raster<C>
trim_border (raster<C> r, int lb, int bb, int rb, int tb) {
  int x1= lb - r->ox, y1= bb - r->oy;
  int x2= r->w - rb - r->ox, y2= r->h - tb - r->oy;
  return subraster (r, x1, y1, x2, y2);
}

template<typename C> raster<C>
trim_border (raster<C> r, int b) {
  return trim_border (r, b, b, b, b);
}

template<typename C> raster<C>
extend_border (raster<C> r, int lb, int bb, int rb, int tb) {
  return change_extents (r, r->w + lb + rb, r->h + bb + tb,
                         r->ox + lb, r->oy + bb);
}

template<typename C> raster<C>
extend_border (raster<C> r, int b) {
  return extend_border (r, b, b, b, b);
}

/******************************************************************************
* Mappers
******************************************************************************/

template<typename Op, typename C> void
foreach (raster<C>& r) {
  int w= r->w, h= r->h, n= w*h;
  for (int i=0; i<n; i++)
    Op::set_op (r->a[i]);
}

template<typename Op, typename C>
raster<Unary_return_type(Op,C) >
map (raster<C> r) {
  typedef Unary_return_type(Op,C) Ret;
  int w= r->w, h= r->h, n= w*h;
  raster<Ret> ret (w, h, r->ox, r->oy);
  for (int i=0; i<n; i++)
    ret->a[i]= Op::op (r->a[i]);
  return ret;
}

template<typename C, typename S> raster<C>
map (unary_function<C,S> fun, raster<S> r) {
  int w= r->w, h= r->h, n= w*h;
  raster<C> ret (w, h, r->ox, r->oy);
  for (int i=0; i<n; i++)
    ret->a[i]= fun->eval (r->a[i]);
  return ret;
}

template<typename Op, typename C, typename S> raster<C>
map (raster<C> r1, raster<S> r2) {
  int w= r1->w, h= r1->h, n= w*h;
  ASSERT (r2->w == w && r2->h == h, "sizes don't match");
  ASSERT (r2->ox == r1->ox && r2->oy == r1->oy, "offsets don't match");
  raster<C> ret (w, h, r1->ox, r1->oy);
  for (int i=0; i<n; i++)
    ret->a[i]= Op::op (r1->a[i], r2->a[i]);
  return ret;
}

template<typename Op, typename C, typename S> raster<C>
map_scalar (raster<C> r, S sc) {
  int w= r->w, h= r->h, n= w*h;
  raster<C> ret (w, h, r->ox, r->oy);
  for (int i=0; i<n; i++)
    ret->a[i]= Op::op (r->a[i], sc);
  return ret;
}

template<typename C> inline void
clear (raster<C>& r) { foreach<clear_op> (r); }
template<typename C> inline void
clear_alpha (raster<C>& r) { foreach<clear_alpha_op> (r); }

template<typename C> inline raster<C>
copy (const raster<C>& r) { return map<copy_op> (r); }
template<typename C> inline raster<C>
mul_alpha (const raster<C>& r) { return map<mul_alpha_op> (r); }
template<typename C> inline raster<C>
div_alpha (const raster<C>& r) { return map<div_alpha_op> (r); }
template<typename C> inline raster<C>
normalize (const raster<C>& r) { return map<normalize_op> (r); }
template<typename C> inline raster<typename C::scalar_type>
get_alpha (const raster<C>& r) { return map<get_alpha_op> (r); }

template<typename C, typename S> inline raster<C>
operator + (const raster<C>& r1, const raster<S>& r2) {
  return map<add_op> (r1, r2); }
template<typename C, typename S> inline raster<C>
operator - (const raster<C>& r1, const raster<S>& r2) {
 return map<sub_op> (r1, r2); }
template<typename C, typename S> inline raster<C>
operator * (const raster<C>& r1, const raster<S>& r2) {
  return map<mul_op> (r1, r2); }
template<typename C, typename S> inline raster<C>
operator / (const raster<C>& r1, const raster<S>& r2) {
  return map<div_op> (r1, r2); }
template<typename C, typename S> inline raster<C>
operator + (const raster<C>& r, const S& sc) {
  return map_scalar<add_op> (r, sc); }
template<typename C, typename S> inline raster<C>
operator - (const raster<C>& r, const S& sc) {
  return map_scalar<sub_op> (r, sc); }
template<typename C, typename S> inline raster<C>
operator - (const S& sc, const raster<C>& r) {
  return map_scalar<neg_sub_op> (r, sc); }
template<typename C, typename S> inline raster<C>
operator * (const raster<C>& r, const S& sc) {
  return map_scalar<mul_op> (r, sc); }
template<typename C, typename S> inline raster<S>
operator * (const S& sc, const raster<C>& r) {
  return map_scalar<mul_op> (r, sc); }
template<typename C, typename S> inline raster<C>
operator / (const raster<C>& r, const S& sc) {
  return map_scalar<div_op> (r, sc); }
template<typename C, typename S> inline raster<C>
copy_alpha (const raster<C>& r1, const raster<S>& r2) {
  return map<copy_alpha_op> (r1, r2); }
template<typename C, typename S> inline raster<C>
apply_alpha (const raster<C>& r1, const raster<S>& r2) {
  return map<apply_alpha_op> (r1, r2); }

template<typename C> inline raster<C>
hypot (const raster<C>& r1, const raster<C>& r2) {
  return map<hypot_op> (r1, r2); }
template<typename C> inline raster<C>
min (const raster<C>& r1, const raster<C>& r2) {
  return map<min_op> (r1, r2); }
template<typename C> inline raster<C>
max (const raster<C>& r1, const raster<C>& r2) {
  return map<max_op> (r1, r2); }

template<typename C, typename S> raster<C>
apply_alpha (C col, raster<S> r) {
  int w= r->w, h= r->h, n= w*h;
  raster<C> ret (w, h, r->ox, r->oy);
  for (int i=0; i<n; i++)
    ret->a[i]= apply_alpha (col, r->a[i]);
  return ret;
}

/******************************************************************************
* Composition
******************************************************************************/

template<composition_mode M, typename C, typename S> void
draw_on (raster<C>& r, S s) {
  int w= r->w, h= r->h, n=w*h;
  for (int i=0; i<n; i++)
    composition_op<M>::set_op (r->a[i], s);
}

template<composition_mode M, typename C, typename S> raster<C>
compose (raster<C> r, S s) {
  int w= r->w, h= r->h, n=w*h;
  raster<C> ret (w, h, r->ox, r->oy);
  for (int i=0; i<n; i++)
    ret->a[i]= composition_op<M>::op (r->a[i], s);
  return ret;
}

template<typename C, typename S> void
draw_on (raster<C>& r, S s, composition_mode mode) {
  switch (mode) {
  case compose_destination:
    draw_on<compose_destination> (r, s);
    break;
  case compose_source:
    draw_on<compose_source> (r, s);
    break;
  case compose_source_over:
    draw_on<compose_source_over> (r, s);
    break;
  case compose_towards_source:
    draw_on<compose_towards_source> (r, s);
    break;
  case compose_alpha_distance:
    draw_on<compose_alpha_distance> (r, s);
    break;
  case compose_add:
    draw_on<compose_add> (r, s);
    break;
  case compose_sub:
    draw_on<compose_sub> (r, s);
    break;
  case compose_mul:
    draw_on<compose_mul> (r, s);
    break;
  case compose_min:
    draw_on<compose_min> (r, s);
    break;
  case compose_max:
    draw_on<compose_max> (r, s);
    break;
  default:
    break;
  }
}

template<typename C, typename S> raster<C>
compose (raster<C> r, S s, composition_mode mode) {
  raster<C> ret= map<copy_op> (r);
  draw_on (ret, s, mode);
  return ret;
}

template<composition_mode M, typename C, typename S> void
draw_on (raster<C>& dest, raster<S> src, int x, int y) {
  x -= src->ox - dest->ox;
  y -= src->oy - dest->oy;
  int dw= dest->w, dh= dest->h;
  int sw= src ->w, sh= src ->h;
  C* d= dest->a;
  S* s= src ->a;
  int sw2= sw;
  int sh2= sh;
  if (x < 0) { s -= x; sw2 += x; x= 0; }
  if (y < 0) { s -= y * sw; sh2 += y; y= 0; }
  int w = min (sw2, dw - x);
  int h = min (sh2, dh - y);
  if (w <= 0 || h <= 0) return;
  d += y * dw + x;
  for (int yy=0; yy<h; yy++, d += dw, s +=sw)
    for (int xx=0; xx<w; xx++)
      composition_op<M>::set_op (d[xx], s[xx]);
}

template<typename C, typename S> void
draw_on (raster<C>& r, raster<S> s, int x, int y, composition_mode mode) {
  switch (mode) {
  case compose_destination:
    draw_on<compose_destination> (r, s, x, y);
    break;
  case compose_source:
    draw_on<compose_source> (r, s, x, y);
    break;
  case compose_source_over:
    draw_on<compose_source_over> (r, s, x, y);
    break;
  case compose_towards_source:
    draw_on<compose_towards_source> (r, s, x, y);
    break;
  case compose_alpha_distance:
    draw_on<compose_alpha_distance> (r, s, x, y);
    break;
  case compose_add:
    draw_on<compose_add> (r, s, x, y);
    break;
  case compose_sub:
    draw_on<compose_sub> (r, s, x, y);
    break;
  case compose_mul:
    draw_on<compose_mul> (r, s, x, y);
    break;
  case compose_min:
    draw_on<compose_min> (r, s, x, y);
    break;
  case compose_max:
    draw_on<compose_max> (r, s, x, y);
    break;
  default:
    break;
  }
}

template<typename C, typename S> raster<C>
empty_join (raster<C> r1, raster<S> r2, composition_mode mode) {
  int w1 = r1->w , h1 = r1->h;
  int ox1= r1->ox, oy1= r1->oy;
  int w2 = r2->w , h2 = r2->h;
  int ox2= r2->ox, oy2= r2->oy;
  int x1=0, y1=0, x2=0, y2=0;
  switch (composition_type (mode)) {
  case 0:
    x1= max (-ox1, -ox2);
    y1= max (-oy1, -oy2);
    x2= min (w1-ox1, w2-ox2);
    y2= min (h1-oy1, h2-oy2);
    x1= min (x1, x2);
    y1= min (y1, y2);
    break;
  case 1:
    x1= -ox2;
    y1= -oy2;
    x2= w2-ox2;
    y2= h2-oy2;
    break;
  case 2:
    x1= -ox1;
    y1= -oy1;
    x2= w1-ox1;
    y2= h1-oy1;
    break;
  case 3:
    x1= min (-ox1, -ox2);
    y1= min (-oy1, -oy2);
    x2= max (w1-ox1, w2-ox2);
    y2= max (h1-oy1, h2-oy2);
    break;
  }
  int w  = x2 - x1;
  int h  = y2 - y1;
  raster<C> ret (w, h, -x1, -y1);
  clear (ret);
  return ret;
}

template<composition_mode M, typename C, typename S> raster<C>
compose (raster<C> r1, raster<S> r2) {
  raster<C> ret= empty_join (r1, r2, M);
  draw_on<compose_source> (ret, r1, 0, 0);
  draw_on<M> (ret, r2, 0, 0);
  return ret;
}

template<typename C, typename S> raster<C>
compose (raster<C> r1, raster<S> r2, composition_mode mode) {
  raster<C> ret= empty_join (r1, r2, mode);
  draw_on (ret, r1, 0, 0, compose_source);
  draw_on (ret, r2, 0, 0, mode);
  return ret;
}

template<typename C> raster<C>
empty_join (array<raster<C> > rs, composition_mode mode) {
  ASSERT (0 < N(rs), "at least one raster expected");
  if (N(rs) == 1) return rs[0];
  if (N(rs) == 2) return empty_join (rs[0], rs[1], mode);
  int m= N(rs) / 2;
  return empty_join (empty_join (range (rs, 0, m), mode),
                     empty_join (range (rs, m, N(rs)), mode), mode);
}

template<typename C> raster<C>
compose (array<raster<C> > rs, composition_mode mode) {
  ASSERT (0 < N(rs), "at least one raster expected");
  if (N(rs) == 1) return rs[0];
  raster<C> ret= empty_join (rs, mode);
  draw_on (ret, rs[0], 0, 0, compose_source);
  for (int i=1; i<N(rs); i++)
    draw_on (ret, rs[i], 0, 0, mode);
  return ret;
}

template<typename C, typename F> raster<C>
mix (raster<C> r1, F a1, raster<C> r2, F a2) {
  raster<C> j= empty_join (r1, r2, compose_add);
  r1= change_extents (r1, j->w, j->h, j->ox, j->oy);
  r2= change_extents (r2, j->w, j->h, j->ox, j->oy);
  for (int i=0; i<j->w*j->h; i++)
    j->a[i]= mix (r1->a[i], a1, r2->a[i], a2);
  return j;
}

/******************************************************************************
* Transformations
******************************************************************************/

template<typename C, typename F> raster<C>
inverse_transform (raster<C> r, F fun, int w, int h, int ox, int oy) {
  raster<C> ret (w, h, ox, oy);
  for (int y=0; y<h; y++)
    for (int x=0; x<w; x++) {
      double xx= x - ox + 0.5;
      double yy= y - oy + 0.5;
      fun.transform (xx, yy);
      ret->a[w*y+x]= r->smooth_pixel (xx, yy);
    }
  return ret;
}

struct shifter {
  double dx, dy;
  inline shifter (double dx2, double dy2): dx (dx2), dy (dy2) {}
  inline void transform (double& x, double& y) { x += dx; y += dy; }
};

template<typename C> raster<C>
shift (raster<C> r, double dx, double dy) {
  shifter sh (-dx, -dy);
  int idx= (int) floor (dx);
  int idy= (int) floor (dy);
  int w  = (idx == dx? r->w: r->w + 1);
  int h  = (idy == dy? r->h: r->h + 1);
  return inverse_transform (r, sh, w, h, r->ox - idx, r->oy - idy);
}

struct magnifier {
  double sx, sy;
  inline magnifier (double sx2, double sy2): sx (sx2), sy (sy2) {}
  inline void transform (double& x, double& y) { x *= sx; y *= sy; }
};

template<typename C> raster<C>
magnify (raster<C> r, double sx, double sy) {
  magnifier mf (1.0/sx, 1.0/sy);
  int x1 = (int) floor (sx * (-r->ox));
  int x2 = (int) ceil  (sx * (r->w - r->ox));
  int y1 = (int) floor (sy * (-r->oy));
  int y2 = (int) ceil  (sy * (r->h - r->oy));
  if (x1 > x2) { int temp= x1; x1= x2; x2= temp; }
  if (y1 > y2) { int temp= y1; y1= y2; y2= temp; }
  return inverse_transform (r, mf, x2-x1, y2-y1, -x1, -y1);
}

struct bubbler {
  double r, a;
  inline bubbler (double r2, double a2): r (r2 / 3.142), a (a2) {}
  inline void transform (double& x, double& y) {
    x += a * r * sin (x / r); y += a * r * sin (y / r); }
};

template<typename C> raster<C>
bubble (raster<C> r, double rr, double a) {
  bubbler bu (rr, a);
  double R= a * rr / 3.142;
  int x1 = (int) floor ((-r->ox) - R);
  int x2 = (int) ceil  ((r->w - r->ox) + R);
  int y1 = (int) floor ((-r->oy) - R);
  int y2 = (int) ceil  ((r->h - r->oy) + R);
  return inverse_transform (r, bu, x2-x1, y2-y1, -x1, -y1);
}

/******************************************************************************
* Special pens
******************************************************************************/

template<typename C, typename F> raster<C>
pixelize (F fun, int w, int h, int ox, int oy, int shrink= 5) {
  raster<C> ret (w, h, ox, oy);
  double delta = 1 / ((double) shrink);
  for (int y=0; y<h; y++)
    for (int x=0; x<w; x++) {
      double cx= x - ox;
      double cy= y - oy;
      if (shrink == 1)
        ret->a[y*w+x]= fun.eval (cx, cy);
      else {
        C r;
        clear (r);
        for (int j=0; j<shrink; j++) {
          double dx= (((double) j) + 0.5) * delta - 0.5;
          for (int i=0; i<shrink; i++) {
            double dy= (((double) i) + 0.5) * delta - 0.5;
            r += fun.eval (cx + dx, cy + dy);
          }
        }
        ret->a[y*w+x]= r / (shrink * shrink);
      }
    }
  return ret;
}

template<typename C> raster<C>
rectangular_pen (double rx, double ry) {
  rx += 0.5; ry += 0.5;
  int w= (int) (2 * ceil (rx + 0.5) - 1);
  int h= (int) (2 * ceil (ry + 0.5) - 1);
  double rem_x= ((double) w) / 2 - rx;
  double rem_y= ((double) h) / 2 - ry;
  raster<C> ret (w, h, w/2, h/2);
  for (int y=0; y<h; y++) {
    double fy= 1.0;
    if (ry <= 0.5) fy= 2 * ry;
    else if (y == 0 || y == h-1) fy= 1 - rem_y;
    for (int x=0; x<w; x++) {
      double fx= 1.0;
      if (rx <= 0.5) fx= 2 * rx;
      else if (x == 0 || x == w-1) fx= 1 - rem_x;
      ret->a[y*w+x]= C (fx * fy);
    }
  }
  return ret;
}

struct chi_rectangular {
  double xx, xy, yx, yy;
  inline chi_rectangular (double rx, double ry, double phi):
    xx ( cos (phi) / rx), xy (sin (phi) / rx),
    yx (-sin (phi) / ry), yy (cos (phi) / ry) {}
  inline double eval (double x, double y) {
    double tx= xx * x + xy * y, ty= yx * x + yy * y;
    if (fabs (tx) < 1 && fabs (ty) < 1) return 1.0;
    else return 0.0; }
};

template<typename C> raster<C>
rectangular_pen (double rx, double ry, double phi) {
  if (fabs (phi) <= 1.0e-6) return rectangular_pen<C> (rx, ry);
  rx += 0.5; ry += 0.5;
  chi_rectangular fun (rx, ry, phi);
  int R= (int) ceil (sqrt (rx * rx + ry * ry) - 0.5);
  int w= (int) (2*R + 1);
  return pixelize<C> (fun, w, w, R, R);  
}

struct chi_oval {
  double xx, xy, yx, yy;
  inline chi_oval (double rx, double ry, double phi):
    xx ( cos (phi) / rx), xy (sin (phi) / rx),
    yx (-sin (phi) / ry), yy (cos (phi) / ry) {}
  inline double eval (double x, double y) {
    double tx= xx * x + xy * y, ty= yx * x + yy * y;
    if (tx * tx + ty * ty < 1) return 1.0;
    else return 0.0; }
};

template<typename C> raster<C>
oval_pen (double rx, double ry, double phi) {
  rx += 0.5; ry += 0.5;
  chi_oval fun (rx, ry, phi);
  int R= (int) ceil (max (rx, ry) - 0.5);
  int w= (int) (2*R + 1);
  return pixelize<C> (fun, w, w, R, R);
}

struct chi_motion {
  double xx, xy, yx, yy, r;
  inline chi_motion (double r2, double phi):
    xx ( cos (phi)), xy (sin (phi)),
    yx (-sin (phi)), yy (cos (phi)), r (r2) {}
  inline double eval (double x, double y) {
    double tx= xx * x + xy * y, ty= yx * x + yy * y;
    if (tx > 0.71 && tx < r+0.71 && fabs (ty) < 0.71) return 1.0;
    else return 0.0; }
};

template<typename C> raster<C>
motion_pen (double dx, double dy) {
  double phi= atan2 (dy, dx);
  double r  = hypot (dx, dy);
  if (r <= 1.0e-6) return rectangular_pen<C> (0.0, 0.0, 0.0);
  chi_motion fun (r, phi);
  int x1= (int) floor (min (dx, 0.0) - 0.5);
  int y1= (int) floor (min (dy, 0.0) - 0.5);
  int x2= (int) ceil  (max (dx, 0.0) + 0.5);
  int y2= (int) ceil  (max (dy, 0.0) + 0.5);
  return pixelize<C> (fun, x2 - x1, y2 - y1, -x1, -y1);
}

/******************************************************************************
* Sum and average
******************************************************************************/

template<typename C> C
sum (raster<C> ras) {
  C ret;
  clear (ret);
  int n= ras->w * ras->h;
  for (int i=0; i<n; i++)
    ret += ras->a[i];
  return ret;
}

template<typename C> C
average (raster<C> ras) {
  typedef typename C::scalar_type R;
  C num;
  R den;
  clear (num);
  clear (den);
  int n= ras->w * ras->h;
  for (int i=0; i<n; i++) {
    num += get_alpha (ras->a[i]) * ras->a[i];
    den += get_alpha (ras->a[i]);
  }
  if (den <= R(0)) return R(0) * num;
  return num / den;
}

/******************************************************************************
* Special convolution kernels
******************************************************************************/

struct gaussian_distribution {
  double xx, xy, yx, yy;
  inline gaussian_distribution (double rx, double ry, double phi):
    xx ( cos (phi) / rx), xy (sin (phi) / rx),
    yx (-sin (phi) / ry), yy (cos (phi) / ry) {}
  inline double eval (double x, double y) {
    double tx= xx * x + xy * y, ty= yx * x + yy * y;
    return exp (- (tx*tx + ty*ty)); }
};

template<typename C> raster<C>
gaussian_pen (double rx, double ry, double phi, double order= 2.5) {
  gaussian_distribution fun (rx, ry, phi);
  double Rx= rx * order, Ry= ry * order;
  int R= (int) ceil (max (Rx, Ry) - 0.5);
  int w= (int) (2*R + 1);
  return pixelize<C> (fun, w, w, R, R, 1);
}

/******************************************************************************
* Convolution and blur
******************************************************************************/

template<typename C, typename S> raster<C>
convolute (raster<C> s1, raster<S> s2) {
  if (s1->w * s1->h == 0) return s1;
  ASSERT (s2->w * s2->h != 0, "empty convolution argument");
  int s1w= s1->w, s1h= s1->h, s2w= s2->w, s2h= s2->h;
  int dw= s1w + s2w - 1, dh= s1h + s2h - 1;
  raster<C> d (dw, dh, s1->ox + s2->ox, s1->oy + s2->oy);
  clear (d);
  raster<C> temp= mul_alpha (s1);
  for (int y1=0; y1<s1h; y1++)
    for (int y2=0; y2<s2h; y2++) {
      int o1= y1 * s1w, o2= y2 * s2w, o= (y1 + y2) * dw;
      for (int x1=0; x1<s1w; x1++)
        for (int x2=0; x2<s2w; x2++)
          d->a[o+x1+x2] += temp->a[o1+x1] * s2->a[o2+x2];
    }
  return div_alpha (d);
}

template<typename C> bool
can_be_factored (raster<C> s) {
  raster<C> xs (s->w, 1, s->ox, 0);
  raster<C> ys (1, s->h, 0, s->oy);
  clear (xs);
  clear (ys);
  for (int x=0; x<s->w; x++)
    for (int y=0; y<s->h; y++) {
      int o= y * s->w;
      xs->a[x] += s->a[o+x];
      ys->a[y] += s->a[o+x];
    }
  for (int x=0; x<s->w; x++)
    for (int y=0; y<s->h; y++) {
      int o= y * s->w;
      if (fabs (s->a[o+x] - xs->a[x] * ys->a[y]) > 0.005) return false;
    }
  return true;
}

template<typename C, typename S> raster<C>
factored_convolute (raster<C> s1, raster<S> s2) {
  if (s1->w * s1->h == 0) return s1;
  ASSERT (s2->w * s2->h != 0, "empty convolution argument");
  int s1w= s1->w, s1h= s1->h, s2w= s2->w, s2h= s2->h;
  raster<S> xs (s2w, 1, s2->ox, 0);
  raster<S> ys (1, s2h, 0, s2->oy);
  clear (xs);
  clear (ys);
  for (int x2=0; x2<s2w; x2++)
    for (int y2=0; y2<s2h; y2++) {
      int o2= y2 * s2w;
      xs->a[x2] += s2->a[o2+x2];
      ys->a[y2] += s2->a[o2+x2];
    }
  int dw= s1w + s2w - 1, dh= s1h + s2h - 1;
  raster<C> temp= mul_alpha (s1);
  raster<C> aux (dw, s1h, s1->ox + s2->ox, s1->oy);
  clear (aux);
  for (int y1=0; y1<s1h; y1++) {
    int o1= y1 * s1w, o= y1 * dw;
    for (int x1=0; x1<s1w; x1++)
      for (int x2=0; x2<s2w; x2++)
        aux->a[o+x1+x2] += temp->a[o1+x1] * xs->a[x2];
  }
  raster<C> d (dw, dh, s1->ox + s2->ox, s1->oy + s2->oy);
  clear (d);
  for (int y1=0; y1<s1h; y1++)
    for (int y2=0; y2<s2h; y2++) {
      int o1= y1 * dw, o= (y1 + y2) * dw;
      for (int x1=0; x1<dw; x1++)
        d->a[o+x1] += aux->a[o1+x1] * ys->a[y2];
    }
  return div_alpha (d);
}

template<typename C> raster<C>
blur (raster<C> ras, raster<double> pen) {
  raster<double> npen= pen / sum (pen);
  if (can_be_factored (npen)) return factored_convolute (ras, npen);
  else return convolute (ras, npen);
}

template<typename C> raster<C>
gaussian_blur (raster<C> ras, double rx, double ry, double phi,
               double order= 2.5) {
  return blur (ras, gaussian_pen<double> (rx, ry, phi, order));
}

template<typename C> raster<C>
gaussian_blur (raster<C> ras, double r) {
  if (r <= 0.001) return ras;
  return gaussian_blur (ras, r, r, 0.0);
}

/******************************************************************************
* Thickening using a pen
******************************************************************************/

template<typename C> inline void
src_over (C& a1, const C& a2) {
  a1= a2 + a1 * (1 - a2);
  //a1= max (a1, a2);
}

template<typename C, typename S> raster<C>
thicken (raster<C> s1, raster<S> s2) {
  typedef typename C::scalar_type F;
  if (s1->w * s1->h == 0) return s1;
  ASSERT (s2->w * s2->h != 0, "empty pen");
  raster<C> d= convolute (s1, s2);
  int s1w= s1->w, s1h= s1->h, s2w= s2->w, s2h= s2->h, dw= d->w;
  raster<F> temp= get_alpha (s1);
  clear_alpha (d);
  for (int y1=0; y1<s1h; y1++)
    for (int y2=0; y2<s2h; y2++) {
      int o1= y1 * s1w, o2= y2 * s2w, o= (y1 + y2) * dw;
      for (int x1=0; x1<s1w; x1++)
        for (int x2=0; x2<s2w; x2++)
          src_over (get_alpha (d->a[o+x1+x2]),
                    temp->a[o1+x1] * s2->a[o2+x2]);
    }
  return d;
}

template<typename C> raster<C>
rectangular_thicken (raster<C> ras, double dx, double dy, double phi) {
  raster<double> pen=
    rectangular_pen<double> (dx / 2 + 0.5, dy / 2 + 0.5, phi);
  return thicken (ras, pen);
}

template<typename C> raster<C>
oval_thicken (raster<C> ras, double dx, double dy, double phi) {
  raster<double> pen= oval_pen<double> (dx / 2 + 0.5, dy / 2 + 0.5, phi);
  return thicken (ras, pen);
}

/******************************************************************************
* Eroding using a pen
******************************************************************************/

template<typename C> inline void
erode (C& dest_a, const C& src_a, const C& pen_a) {
  C a= src_a * pen_a + (1 - pen_a);
  dest_a= min (dest_a, a);
}

template<typename C, typename S> raster<C>
erode (raster<C> s1, raster<S> s2) {
  typedef typename C::scalar_type F;
  if (s1->w * s1->h == 0) return s1;
  ASSERT (s2->w * s2->h != 0, "empty pen");
  raster<C> d= copy (s1);
  int s1w= s1->w, s1h= s1->h, s2w= s2->w, s2h= s2->h, dw= d->w;//, dh= d->h;
  raster<F> temp= get_alpha (s1);
  //for (int i=0; i<dw*dh; i++)
  //  get_alpha (d->a[i])= F (1.0);
  for (int y1=0; y1<s1h; y1++)
    for (int y2=0; y2<s2h; y2++) {
      int yd= y1 + y2 - s2->oy;
      if (yd < 0 || yd >= s1h) continue;
      int o1= y1 * s1w, o2= y2 * s2w, o= yd * dw;
      for (int x1=0; x1<s1w; x1++)
        for (int x2=0; x2<s2w; x2++) {
          int xd= x1 + x2 - s2->ox;
          if (xd < 0 || xd >= s1w) continue;
          erode (get_alpha (d->a[o+xd]), temp->a[o1+x1], s2->a[o2+x2]);
        } 
    }
  return d;
}

/******************************************************************************
* Inner variation
******************************************************************************/

template<typename C, typename S> raster<C>
variation (raster<C> s1, raster<S> s2) {
  typedef typename C::scalar_type F;
  if (s1->w * s1->h == 0) return s1;
  ASSERT (s2->w * s2->h != 0, "empty pen");
  raster<C> d= convolute (s1, s2);
  int s2w= s2->w, s2h= s2->h, dw= d->w, dh= d->h;
  int s2ox= s2->ox, s2oy= s2->oy;
  raster<F> temp= get_alpha (s1);
  for (int y0=0; y0<dh; y0++)
    for (int x0=0; x0<dw; x0++) {
      int x1= x0 - s2ox, y1= y0 - s2oy;
      F ref= temp->internal_get_pixel (x1, y1);
      F min_v= 0, max_v= 0;
      for (int y2=0; y2<s2h; y2++)
        for (int x2=0; x2<s2w; x2++) {
          F cur= temp->internal_get_pixel (x1 - (x2 - s2ox), y1 - (y2 - s2oy));
          F v= (cur - ref) * s2->a[y2*s2w + x2];
          max_v= max (max_v, v);
          min_v= min (min_v, v);
        }
      get_alpha (d->a[y0*dw + x0]) = max_v - min_v;
    }
  return d;
}

template<typename C> raster<C>
rectangular_variation (raster<C> ras, double dx, double dy, double phi) {
  raster<double> pen=
    rectangular_pen<double> (dx / 2 + 0.5, dy / 2 + 0.5, phi);
  return variation (ras, pen);
}

template<typename C> raster<C>
oval_variation (raster<C> ras, double dx, double dy, double phi) {
  raster<double> pen= oval_pen<double> (dx / 2 + 0.5, dy / 2 + 0.5, phi);
  return variation (ras, pen);
}

/******************************************************************************
* Gravitational effects
******************************************************************************/

template<typename C> raster<C>
gravitation (int R, double expon, bool y_flag) {
  int w= 2*R+1, h= 2*R+1;
  raster<C> ret (w, h, R, R);
  for (int y=0; y<h; y++) {
    double sq_y= (y-R)*(y-R);
    for (int x=0; x<w; x++)
      if (x == R && y == R) ret->a[y*w+x]= 0.0;
      else {
        double sq_x= (x-R)*(x-R);
        double sq_r= sq_x + sq_y;
        double r   = sqrt (sq_r);
        double u   = (y_flag? (y-R): (x-R)) / r;
        ret->a[y*w+x]= u / pow (r, expon);
      }
  }
  return ret;
}

template<typename C> typename C::scalar_type
inner_max (raster<C> r, C s) {
  typedef typename C::scalar_type F;
  int w= r->w, h= r->h;
  F ret= 0.001;
  for (int y=0; y<h; y++)
    for (int x=0; x<w; x++)
      ret= max (ret, inner_max (r->a[y*w+x], s));
  return ret;
}

template<typename C, typename S> raster<C>
divide (raster<C> r, S s) {
  int w= r->w, h= r->h;
  raster<C> ret (w, h, r->ox, r->oy);
  for (int y=0; y<h; y++)
    for (int x=0; x<w; x++)
      ret->a[y*w+x]= r->a[y*w+x] / s;
  return ret;
}

template<typename C> raster<C>
gravitational_outline (raster<C> s, int R, double expon) {
  typedef typename C::scalar_type F;
  raster<F> gravx= gravitation<F> (R, expon, false);
  raster<F> gravy= gravitation<F> (R, expon, true );
  raster<C> convx= convolute (s, gravx);
  raster<C> convy= convolute (s, gravy);
  raster<C> d= hypot (convx, convy);
  F mc= inner_max (d, C (1.0, 1.0, 1.0, 0.0));
  F ma= inner_max (d, C (0.0, 0.0, 0.0, 1.0));
  C sc (mc, mc, mc, ma);
  d= divide (d, sc);
  return normalize (d);
}

/******************************************************************************
* Gravitational engrave
******************************************************************************/

template<typename C> raster<C>
incidence (raster<C> gx, raster<C> gy, double phi) {
  int w= gx->w, h= gx->h;
  raster<C> res (w, h, gx->ox, gx->oy);
  double ca= cos (phi), sa= sin (phi);
  for (int i=0; i<w*h; i++) {
    C xa= gx->a[i];
    C ya= gy->a[i];
    C r = 1.0e-100 + sqrt (xa*xa + ya*ya);
    C pr= (xa * ca + ya * sa) / r;
    res->a[i]= sqrt (max (pr, 0.0));
  }
  return res;
}

template<typename C, typename F> raster<C>
mul_alpha (C c, raster<F> alpha) {
  int w= alpha->w, h= alpha->h, n= w*h;
  raster<C> ret (w, h, alpha->ox, alpha->oy);
  for (int i=0; i<n; i++)
    ret->a[i]= C (c.r, c.g, c.b, c.a * alpha->a[i]);
  return ret;
}

template<typename C> raster<C>
gravitational_shadow (raster<C> s, int R, double expon,
                      color col, double phi) {
  typedef typename C::scalar_type F;
  raster<F> als  = get_alpha (s);
  raster<F> gravx= gravitation<F> (R, expon, false);
  raster<F> gravy= gravitation<F> (R, expon, true );
  raster<F> convx= trim_border (convolute (als, gravx), R);
  raster<F> convy= trim_border (convolute (als, gravy), R);
  raster<F> incid= incidence (convx, convy, phi);
  raster<C> ret  = copy (s);
  raster<C> shad = mul_alpha (C (col), incid * als);
  draw_on<compose_source_over,C,C> (ret, shad, 0, 0);
  return ret;
}

/******************************************************************************
* Edge distances
******************************************************************************/

template<typename C> raster<double>
tl_distances (raster<C> r) {
  int w= r->w, h= r->h;
  raster<double> ret (w, h, r->ox, r->oy);
  for (int y=h-1; y>=0; y--)
    for (int x=0; x<w; x++) {
      double a = get_alpha (r->a[y*w+x]);
      double px= (x>0? ret->a[y*w+x-1]: 0.0);
      double py= (y+1<h? ret->a[(y+1)*w+x]: 0.0);
      if (a == 0.0) ret->a[y*w+x]= 0.0;
      else ret->a[y*w+x]= min (px, py) + a;
    }
  return ret;
}

template<typename C> raster<double>
br_distances (raster<C> r) {
  int w= r->w, h= r->h;
  raster<double> ret (w, h, r->ox, r->oy);
  for (int y=0; y<h; y++)
    for (int x=w-1; x>=0; x--) {
      double a= get_alpha (r->a[y*w+x]);
      double px= (x+1<w? ret->a[y*w+x+1]: 0.0);
      double py= (y>0? ret->a[(y-1)*w+x]: 0.0);
      if (a == 0.0) ret->a[y*w+x]= 0.0;
      else ret->a[y*w+x]= min (px, py) + a;
    }
  return ret;
}

/******************************************************************************
* Random Perlin noise
******************************************************************************/

class true_color;

raster<double>
turbulence (int w, int h, int ox, int oy, long seed,
            double wavelen_x, double wavelen_y, 
            int nNumOctaves, bool bFractalSum);

raster<double>
turbulence (raster<double> ras, long seed,
            double wavelen_x, double wavelen_y, 
            int nNumOctaves, bool bFractalSum);

raster<true_color>
turbulence (raster<true_color> ras, long seed,
            double wavelen_x, double wavelen_y,
            int nNumOctaves, bool bFractalSum);

/******************************************************************************
* Degrading
******************************************************************************/

template<typename C> raster<C>
degrade (raster<C> r, double wlx, double wly, double th, double sh) {
  th= min (max (th, 1.0e-6), 1.0 - 1.0e-6);
  double delta= 1.0 / (1.0 + max (10.0 * sh, 0.0));
  double a1= max (th - delta, 0.0);
  double a2= min (th + delta, 1.0);
  raster<double> a= turbulence (r->w, r->h, r->ox, r->oy, 12321,
                                wlx, wly, 3, true);
  int n= r->w * r->h;
  for (int i=0; i<n; i++) {
    double v= a->a[i];
    if (v <= a1) a->a[i]= 1.0;
    else if (v >= a2) a->a[i]= 0.0;
    else a->a[i]= (a2 - v) / (a2 - a1);
  }
  return apply_alpha (r, a);
}

/******************************************************************************
* Translations
******************************************************************************/

template<typename C> raster<C>
translate (raster<C> r, raster<double> rdx, raster<double> rdy,
           double rx, double ry, bool tiled= false) {
  ASSERT (rdx->w == r->w && rdx->h == r->h, "incompatible dx dimensions");
  ASSERT (rdy->w == r->w && rdy->h == r->h, "incompatible dy dimensions");
  int w= r->w, h= r->h;
  raster<C> ret (w, h, r->ox, r->oy);
  for (int y=0; y<h; y++)
    for (int x=0; x<w; x++) {
      double dx= rx * (rdx->a[y*w+x] - 0.5);
      double dy= ry * (rdy->a[y*w+x] - 0.5);
      double idx= floor (dx);
      double idy= floor (dy);
      double fdx= dx - idx;
      double fdy= dy - idy;
      int xx= x + (int) idx;
      int yy= y + (int) idy;
      C v00, v01, v10, v11;
      if (xx >= 0 && xx+1 < w && yy >= 0 && yy+1 < h) {
        v00= r->a[yy*w+xx];
        v10= r->a[yy*w+xx+1];
        v01= r->a[yy*w+xx+w];
        v11= r->a[yy*w+xx+w+1];
      }
      else if (tiled) {
        while (xx < 0) xx += w;
        if (xx >= w) xx= xx % w;
        while (yy < 0) yy += h;
        if (yy >= h) yy= yy % h;
        int xx1= (xx+1<w? xx+1: 0);
        int yy1= (yy+1<h? yy+1: 0);
        v00= r->a[yy  * w + xx ];
        v10= r->a[yy  * w + xx1];
        v01= r->a[yy1 * w + xx ];
        v11= r->a[yy1 * w + xx1];
      }
      else {
        if (xx >= 0 && xx < w && yy >= 0 && yy < h) v00= r->a[yy*w+xx];
        else clear (v00);
        if (xx >= -1 && xx+1 < w && yy >= 0 && yy < h) v10= r->a[yy*w+xx+1];
        else clear (v10);
        if (xx >= 0 && xx < w && yy >= -1 && yy+1 < h) v10= r->a[yy*w+xx+w];
        else clear (v01);
        if (xx >= -1 && xx+1<w && yy >= -1 && yy+1<h) v10= r->a[yy*w+xx+w+1];
        else clear (v11);
      }
      C v0= mix (v00, 1.0-fdx, v01, fdx);
      C v1= mix (v10, 1.0-fdx, v11, fdx);
      C v = mix (v0, 1.0-fdy, v1, fdy);
      ret->a[y*w+x]= v;
      /*
      int xx= x + (int) floor (dx + 0.5);
      int yy= y + (int) floor (dy + 0.5);
      // FIXME: we might wish to interpolate using the fractional parts
      if (xx >= 0 && xx < w && yy >= 0 && yy < h)
        ret->a[y*w+x]= r->a[yy*w+xx];
      else
        clear (ret->a[y*w+x]);
      */
    }
  return ret;
}

template<typename C> raster<C>
distort (raster<C> r, double wlx, double wly, double rx, double ry) {
  int Rx= (int) ceil (fabs (rx));
  int Ry= (int) ceil (fabs (ry));
  r= extend_border (r, Rx, Ry, Rx, Ry);
  raster<double> rdx= turbulence (r->w, r->h, r->ox, r->oy, 12345,
                                  wlx, wly, 3, true);
  raster<double> rdy= turbulence (r->w, r->h, r->ox, r->oy, 54321,
                                  wlx, wly, 3, true);
  return translate (r, rdx, rdy, rx, ry);
}

template<typename C> raster<C>
tiled_distort (raster<C> r, double rx, double ry) {
  raster<double> rdx= turbulence (r->w, r->h, r->ox, r->oy, -12345,
                                  r->w, r->h, 3, true);
  raster<double> rdy= turbulence (r->w, r->h, r->ox, r->oy, -54321,
                                  r->w, r->h, 3, true);
  return translate (r, rdx, rdy, rx, ry, true);
}

template<typename C> raster<C>
gnaw (raster<C> r, double wlx, double wly, double rx, double ry) {
  int Rx= (int) ceil (fabs (rx));
  int Ry= (int) ceil (fabs (ry));
  r= extend_border (r, Rx, Ry, Rx, Ry);
  raster<double> rdx= turbulence (r->w, r->h, r->ox, r->oy, 12345,
                                  wlx, wly, 3, true);
  raster<double> rdy= turbulence (r->w, r->h, r->ox, r->oy, 54321,
                                  wlx, wly, 3, true);
  raster<C> ret= translate (r, rdx, rdy, rx, ry);
  return apply_alpha (ret, get_alpha (r));
}

#endif // defined RASTER_H
