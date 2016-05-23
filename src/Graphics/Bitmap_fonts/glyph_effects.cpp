
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
* distorted font glyphs
******************************************************************************/

glyph
distorted (glyph gl, tree kind, SI em, int c) {
  (void) kind;
  int i, j;
  int ww= gl->width, hh= gl->height;
  glyph bmr (ww, hh, gl->xoff, gl->yoff, gl->depth);
  double wl= 0.1 * (em / PIXEL);
  raster<double> ras= turbulence (ww, hh, 0, 0, c&31, wl, wl, 3, true);
  double threshold= 0.5;
  if (is_tuple (kind, "degraded") && N(kind) >= 2)
    threshold= as_double (kind[1]);
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
