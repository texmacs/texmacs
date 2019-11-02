
/******************************************************************************
* MODULE     : glyph_effects.cpp
* DESCRIPTION: special effects on font glyphs
* COPYRIGHT  : (C) 2016  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "bitmap_font.hpp"
#include "renderer.hpp"
#include "hashset.hpp"
#include "raster.hpp"

extern glyph error_glyph;
extern metric error_metric;

/******************************************************************************
* degraded font glyphs
******************************************************************************/

glyph
degraded (glyph gl, double threshold, double freq, SI em, int c) {
  int i, j;
  int ww= gl->width, hh= gl->height;
  glyph bmr (ww, hh, gl->xoff, gl->yoff, gl->depth);
  double wl= (0.1 / freq) * (em / PIXEL);
  raster<double> ras= turbulence (ww, hh, 0, 0, c&31, wl, wl, 3, true);
  for (j=0; j<hh; j++)
    for (i=0; i<ww; i++) {
      int old_val= gl->get_x (i, j);
      int new_val= old_val;
      if (ras->internal_get_pixel (i, j) > threshold) new_val= 0;
      bmr->set_x (i, j, new_val);
    }
  bmr->lwidth= gl->lwidth;
  return simplify (bmr);
}

/******************************************************************************
* degraded font glyphs
******************************************************************************/

glyph
distorted (glyph gl, double strength, double freq, int gnaw, SI em, int c) {
  double wl= (0.1 / freq) * (em / PIXEL);
  double r= wl * strength;
  int pad= (int) ceil (r);
  if (gnaw >= 0) gl= padded (gl, pad, pad, pad, pad);
  
  int i, j;
  int ww= gl->width, hh= gl->height;
  glyph bmr (ww, hh, gl->xoff, gl->yoff, gl->depth);
  raster<double> dx= turbulence (ww, hh, 0, 0, c&31, wl, wl, 3, true);
  raster<double> dy= turbulence (ww, hh, 0, 0, (c&31) + 32, wl, wl, 3, true);
  for (j=0; j<hh; j++)
    for (i=0; i<ww; i++) {
      double di= r * (dx->internal_get_pixel (i, j) - 0.5);
      double dj= r * (dy->internal_get_pixel (i, j) - 0.5);
      int i2= i + (int) floor (di + 0.5);
      int j2= j + (int) floor (dj + 0.5);
      int val= 0;
      if (i2 >= 0 && i2 < ww && j2 >= 0 && j2 < hh)
        val= gl->get_x (i2, j2);
      if (gnaw < 0) val= min (val, gl->get_x (i, j));
      if (gnaw > 0) val= max (val, gl->get_x (i, j));
      bmr->set_x (i, j, val);
    }
  bmr->lwidth= gl->lwidth;
  return simplify (bmr);
}

/******************************************************************************
* distorted font glyphs
******************************************************************************/

glyph
distorted (glyph gl, tree kind, SI em, int c) {
  if (is_tuple (kind, "degraded") && N(kind) >= 3)
    return degraded (gl, as_double (kind[1]), as_double (kind[2]), em, c);
  if (is_tuple (kind, "distorted") && N(kind) >= 3)
    return distorted (gl, as_double (kind[1]), as_double (kind[2]), 0, em, c);
  if (is_tuple (kind, "gnawed") && N(kind) >= 3)
    return distorted (gl, as_double (kind[1]), as_double (kind[2]), -1, em, c);
  return gl;
}

struct distorted_font_glyphs_rep: public font_glyphs_rep {
  font_glyphs fng;
  tree kind;
  SI em;
  hashmap<int,glyph> gs;
  distorted_font_glyphs_rep (string name, font_glyphs fng2, tree k2, int e2):
    font_glyphs_rep (name), fng (fng2), kind (k2), em (e2), gs (error_glyph) {}
  glyph& get (int c) {
    glyph& orig (fng->get (c));
    if ((&orig != &error_glyph) && !gs->contains (c))
      gs(c)= distorted (orig, kind, em, c);
    return gs(c); }
};

string
functional_to_string (tree t) {
  if (is_atomic (t)) return copy (t->label);
  else {
    int i= 0, n= N(t);
    string s= copy (as_string (L(t)));
    if (is_tuple (t) && n > 0) { s= functional_to_string (t[0]); i++; }
    s << "(";
    for (; i<n; i++) {
      s << functional_to_string (t[i]);
      if (i<n-1) s << ",";
    }
    return s;
  }
}

font_glyphs
distorted (font_glyphs fng, tree kind, SI em) {
  string ks= functional_to_string (kind);
  string ems= as_string (em);
  string name= "distorted[" * fng->res_name * "," * ks * "," * em * "]";
  return make (font_glyphs, name,
               tm_new<distorted_font_glyphs_rep> (name, fng, kind, em));
}
