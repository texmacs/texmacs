
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

template<typename C> class raster {
  ABSTRACT_NULL_TEMPLATE(raster,C);
  inline raster (int w, int h, int ox, int oy):
    rep (tm_new<raster_rep<C> > (w, h, ox, oy)) { INC_COUNT(rep); }
};
ABSTRACT_NULL_TEMPLATE_CODE(raster,class,C);

template<typename C> void
print (const C* s, int w, int h) {
  for (int y=0; y<h; y++)
    for (int x=0; x<w; x++)
      cout << x << ", " << y << " -> " << s[y*w+x] << "\n";
}

template<typename C> void
print (raster<C> r) {
  for (int y=0; y<r->h; y++)
    for (int x=0; x<r->w; x++)
      cout << x << ", " << y << " -> " << r->a[y*r->w+x] << "\n";
}

/******************************************************************************
* Low level routines for raster manipulation
******************************************************************************/

template<typename C> raster<C>
hide_alpha (raster<C> r) {
  int w= r->w, h= r->h;
  raster<C> ret (w, h, r->ox, r->oy);
  for (int y=0; y<h; y++)
    for (int x=0; x<w; x++)
      ret->a[y*w+x]= hide_alpha (r->a[y*w+x]);
  return ret;
}

template<typename C> raster<C>
show_alpha (raster<C> r) {
  int w= r->w, h= r->h;
  raster<C> ret (w, h, r->ox, r->oy);
  for (int y=0; y<h; y++)
    for (int x=0; x<w; x++)
      ret->a[y*w+x]= show_alpha (r->a[y*w+x]);
  return ret;
}

template<typename C> void
clear (raster<C>& r) {
  int w= r->w, h= r->h;
  for (int i=0; i<w*h; i++)
    r->a[i]= C ((color) 0);
}

template<typename C, typename S> raster<C>
convolute (raster<C> s1, raster<S> s2) {
  if (s1->w * s1->h == 0) return s1;
  ASSERT (s2->w * s2->h != 0, "empty convolution argument");
  int s1w= s1->w, s1h= s1->h, s2w= s2->w, s2h= s2->h;
  int dw= s1w + s2w - 1, dh= s1h + s2h - 1;
  raster<C> d (dw, dh, s1->ox + s2->ox, s1->oy + s2->oy);
  clear (d);
  raster<C> temp= hide_alpha (s1);
  for (int y1=0; y1<s1h; y1++)
    for (int y2=0; y2<s2h; y2++) {
      int o1= y1 * s1w, o2= y2 * s2w, o= (y1 + y2) * dw;
      for (int x1=0; x1<s1w; x1++)
        for (int x2=0; x2<s2w; x2++)
          d->a[o+x1+x2] += temp->a[o1+x1] * s2->a[o2+x2];
    }
  return show_alpha (d);
}

/*
template<typename D, typename S1, typename S2> void
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
*/

template<typename C> raster<C>
gaussian (int R, double r) {
  int w= 2*R+1, h= 2*R+1;
  raster<C> ret (w, h, R, R);
  double lambda= 1.0 / (2.0 * acos (0.0) * r * r);
  double sq_r= r*r;
  for (int y=0; y<h; y++) {
    double sq_y= (y-R)*(y-R);
    for (int x=0; x<w; x++) {
      double sq_x= (x-R)*(x-R);
      ret->a[y*w+x]= C (lambda * (exp (- (sq_x + sq_y) / sq_r)));
    }
  }
  return ret;
}

template<typename C> raster<C>
blur (raster<C> ras, int R, double r) {
  raster<double> g= gaussian<double> (R, r);
  return convolute (ras, g);
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

template<typename C> raster<C>
norm (raster<C> s1, raster<C> s2) {
  int w= s1->w, h= s1->h;
  ASSERT (s2->w == w && s2->h == h, "sizes don't match");
  raster<C> ret (w, h, s1->ox, s1->oy);
  for (int y=0; y<h; y++)
    for (int x=0; x<w; x++)
      ret->a[y*w+x]= norm (s1->a[y*w+x], s2->a[y*w+x]);
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
normalize (raster<C> r) {
  int w= r->w, h= r->h;
  raster<C> ret (w, h, r->ox, r->oy);
  for (int y=0; y<h; y++)
    for (int x=0; x<w; x++)
      ret->a[y*w+x]= normalize (r->a[y*w+x]);
  return ret;
}

template<typename C> raster<C>
gravitational_outline (raster<C> s, int R, double expon) {
  typedef typename C::scalar_type F;
  raster<F> gravx= gravitation<F> (R, expon, false);
  raster<F> gravy= gravitation<F> (R, expon, true );
  //int w= s->w, h= s->h;
  //int tw= 2*R+1, th= 2*R+1;
  //raster<C> convx (w + 2*R, h + 2*R, s->ox + R, s->oy + R);
  //raster<C> convy (w + 2*R, h + 2*R, s->ox + R, s->oy + R);
  //convolute (convx->a, s->a, gravx->a, w, h, tw, th);
  //convolute (convy->a, s->a, gravy->a, w, h, tw, th);
  raster<C> convx= convolute (s, gravx);
  raster<C> convy= convolute (s, gravy);
  raster<C> d= norm (convx, convy);
  F mc= inner_max (d, C (1.0, 1.0, 1.0, 0.0));
  F ma= inner_max (d, C (0.0, 0.0, 0.0, 1.0));
  C sc (mc, mc, mc, ma);
  d= divide (d, sc);
  return normalize (d);
}

/******************************************************************************
* Low level composition
******************************************************************************/

template<composition_mode M, typename D, typename S>
struct composer {
  static inline void op (D& dest, const S& src) { (void) dest; (void) src; }
};

template<typename D, typename S>
struct composer<compose_source,D,S> {
  static inline void op (D& dest, const S& src) { dest= src; }
};

template<typename D, typename S>
struct composer<compose_source_over,D,S> {
  static inline void op (D& dest, const S& src) { source_over (dest, src); }
};

template<typename D, typename S>
struct composer<compose_towards_source,D,S> {
  static inline void op (D& dest, const S& src) { towards_source (dest, src); }
};

template<composition_mode M, typename D, typename S> void
compose (D* d, const S& s, int w, int h, int wd) {
  for (int y=0; y<h; y++, d += wd)
    for (int x=0; x<w; x++)
      composer<M,D,S>::op (d[x], s);
}

template<composition_mode M, typename D, typename S> void
compose (D* d, const S* s, int w, int h, int wd, int ws) {
  for (int y=0; y<h; y++, d += wd, s +=ws)
    for (int x=0; x<w; x++)
      composer<M,D,S>::op (d[x], s[x]);
}

/******************************************************************************
* Low level edge distances
******************************************************************************/

template<typename C> void
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
