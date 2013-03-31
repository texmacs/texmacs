
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
#include "picture.hpp"

/******************************************************************************
* Raster class
******************************************************************************/

template<class C> class raster;
template<class C> bool is_nil (raster<C> r);

template<class C>
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

  /*
  C get_pixel (int x, int y) {
    if (0 > x || 0 > y || x >= w || y >= h) return transparent<C> ();
    else return (color) a [y*w + x];
  }

  void set_pixel (int x, int y, color c) {
    if (0 > x || 0 > y || x >= w || y >= h);
    else a [y*w + x]= C (c);
  }
  */
};

template<class C> class raster {
  ABSTRACT_NULL_TEMPLATE(raster,C);
  inline raster (int w, int h, int ox, int oy):
    rep (tm_new<raster_rep<C> > (w, h, ox, oy)) { INC_COUNT(rep); }
};
ABSTRACT_NULL_TEMPLATE_CODE(raster,class,C);

template<class C> void
print (const C* s, int w, int h) {
  for (int y=0; y<h; y++)
    for (int x=0; x<w; x++)
      cout << x << ", " << y << " -> " << s[y*w+x] << "\n";
}

/******************************************************************************
* Low level routines for raster manipulation
******************************************************************************/

template<class C> void
hide_alpha (C* d, const C* s, int w, int h) {
  for (int y=0; y<h; y++) {
    int o= y * w;
    for (int x=0; x<w; x++)
      d[o+x]= hide_alpha (s[o+x]);
  }
}

template<class C> void
show_alpha (C* d, const C* s, int w, int h) {
  for (int y=0; y<h; y++) {
    int o= y * w;
    for (int x=0; x<w; x++)
      d[o+x]= show_alpha (s[o+x]);
  }
}

template<class D> void
clear (D* d, int w, int h) {
  for (int i=0; i<w*h; i++)
    d[i]= D ((color) 0);
}

template<class D, class S1, class S2> void
convolute (D* d, const S1* s1, const S2* s2,
           int s1w, int s1h, int s2w, int s2h) {
  if (s1w * s1h == 0) return;
  int dw= s1w + s2w - 1, dh= s1h + s2h - 1;
  clear (d, dw, dh);
  S1* temp= tm_new_array<S1> (s1w * s1h);
  hide_alpha (temp, s1, s1w, s1h);
  for (int y1=0; y1<s1h; y1++)
    for (int y2=0; y2<s2h; y2++) {
      int o1= y1 * s1w, o2= y2 * s2w, o= (y1 + y2) * dw;
      for (int x1=0; x1<s1w; x1++)
        for (int x2=0; x2<s2w; x2++)
          d[o+x1+x2] += temp[o1+x1] * s2[o2+x2];
    }
  show_alpha (d, d, dw, dh);
  tm_delete_array (temp);
}

template<class C> void
gaussian (C* d, int R, double r) {
  int w= 2*R+1, h= 2*R+1;
  double lambda= 1.0 / (2.0 * acos (0.0) * r * r);
  double sq_r= r*r;
  for (int y=0; y<h; y++) {
    double sq_y= (y-R)*(y-R);
    for (int x=0; x<w; x++) {
      double sq_x= (x-R)*(x-R);
      d[y*w+x]= C (lambda * (exp (- (sq_x + sq_y) / sq_r)));
    }
  }
}

template<class C, class F> void
blur (C* d, const C* s, int w, int h, int R, double r) {
  int tw= 2*R+1, th= 2*R+1;
  F* temp= tm_new_array<F> (tw * th);
  gaussian (temp, R, r);
  convolute (d, s, temp, w, h, tw, th);
  tm_delete_array (temp);  
}

/******************************************************************************
* Gravitational effects
******************************************************************************/

template<class C> void
gravitation (C* d, int R, double expon, bool y_flag) {
  int w= 2*R+1, h= 2*R+1;
  for (int y=0; y<h; y++) {
    double sq_y= (y-R)*(y-R);
    for (int x=0; x<w; x++)
      if (x == R && y == R) d[y*w+x]= 0.0;
      else {
        double sq_x= (x-R)*(x-R);
        double sq_r= sq_x + sq_y;
        double r   = sqrt (sq_r);
        double u   = (y_flag? (y-R): (x-R)) / r;
        d[y*w+x]= u / pow (r, expon);
      }
  }
}

template<class C> void
norm (C* d, const C* s1, const C* s2, int w, int h) {
  for (int y=0; y<h; y++)
    for (int x=0; x<w; x++)
      d[y*w+x]= norm (s1[y*w+x], s2[y*w+x]);
}

template<class C, class F> F
max (const C* d, int w, int h, const C& s) {
  F r= 0.001;
  for (int y=0; y<h; y++)
    for (int x=0; x<w; x++)
      r= max (r, inner_max (d[y*w+x], s));
  return r;
}

template<class C, class S> void
divide (C* d, int w, int h, const S& s) {
  for (int y=0; y<h; y++)
    for (int x=0; x<w; x++)
      d[y*w+x]= d[y*w+x] / s;
}

template<class C> void
normalize (C* d, int w, int h) {
  for (int y=0; y<h; y++)
    for (int x=0; x<w; x++)
      d[y*w+x]= normalize (d[y*w+x]);
}

template<class C, class F> void
gravitational_outline (C* d, const C* s, int w, int h, int R, double expon) {
  int tw= 2*R+1, th= 2*R+1;
  int ww= w + tw - 1, hh= h + th - 1;
  F* gravx= tm_new_array<F> (2 * tw * th);
  F* gravy= gravx + tw * th;
  C* convx= tm_new_array<C> (2 * ww * hh);
  C* convy= convx + ww * hh;
  gravitation (gravx, R, expon, false);
  gravitation (gravy, R, expon, true );
  convolute (convx, s, gravx, w, h, tw, th);
  convolute (convy, s, gravy, w, h, tw, th);
  norm (d, convx, convy, ww, hh);
  F mc= max<C,F> (d, ww, hh, C (1.0, 1.0, 1.0, 0.0));
  F ma= max<C,F> (d, ww, hh, C (0.0, 0.0, 0.0, 1.0));
  C sc (mc, mc, mc, ma);
  divide (d, ww, hh, sc);
  normalize (d, ww, hh);
  tm_delete_array (convx);
  tm_delete_array (gravx);
}

/******************************************************************************
* Low level composition
******************************************************************************/

template<composition_mode M, class D, class S>
struct composer {
  static inline void op (D& dest, const S& src) { (void) dest; (void) src; }
};

template<class D, class S>
struct composer<compose_source,D,S> {
  static inline void op (D& dest, const S& src) { dest= src; }
};

template<class D, class S>
struct composer<compose_source_over,D,S> {
  static inline void op (D& dest, const S& src) { source_over (dest, src); }
};

template<class D, class S>
struct composer<compose_towards_source,D,S> {
  static inline void op (D& dest, const S& src) { towards_source (dest, src); }
};

template<composition_mode M, class D, class S> void
compose (D* d, const S& s, int w, int h, int wd) {
  for (int y=0; y<h; y++, d += wd)
    for (int x=0; x<w; x++)
      composer<M,D,S>::op (d[x], s);
}

template<composition_mode M, class D, class S> void
compose (D* d, const S* s, int w, int h, int wd, int ws) {
  for (int y=0; y<h; y++, d += wd, s +=ws)
    for (int x=0; x<w; x++)
      composer<M,D,S>::op (d[x], s[x]);
}

/******************************************************************************
* Low level edge distances
******************************************************************************/

template<class C> void
inner_distances (double* d, const C* s, int w, int h) {
  for (int y=h-1; y>=0; y--)
    for (int x=0; x<w; x++) {
      double a = get_alpha (s[y*w+x]);
      double px= (x>0? d[2*(y*w+x-1) + 0]: 0.0);
      double py= (y+1<h? d[2*((y+1)*w+x) + 0]: 0.0);
      if (a == 0.0) d[2*(y*w+x) + 0]= 0.0;
      else d[2*(y*w+x) + 0]= min (px, py) + a;
    }
  for (int y=0; y<h; y++)
    for (int x=w-1; x>=0; x--) {
      double a= get_alpha (s[y*w+x]);
      double px= (x+1<w? d[2*(y*w+x+1) + 1]: 0.0);
      double py= (y>0? d[2*((y-1)*w+x) + 1]: 0.0);
      if (a == 0.0) d[2*(y*w+x) + 1]= 0.0;
      else d[2*(y*w+x) + 1]= min (px, py) + a;
    }
}

#endif // defined RASTER_H
