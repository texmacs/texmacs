
/******************************************************************************
* MODULE     : glyph_effected.cpp
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
#include "effect.hpp"
#include "raster_picture.hpp"
#include "true_color.hpp"

extern glyph error_glyph;
extern metric error_metric;
extern string functional_to_string (tree t);

/******************************************************************************
* Apply effect to a set of font metrics
******************************************************************************/

struct effected_font_metric_rep: public font_metric_rep {
  font_metric fnm;
  effect eff;
  hashmap<int,pointer> ms;
  effected_font_metric_rep (string name, font_metric fnm2, effect eff2):
    font_metric_rep (name), fnm (fnm2), eff (eff2), ms (error_metric) {}
  bool exists (int c) { return fnm->exists (c); }
  metric& get (int c) {
    metric& m (fnm->get (c));
    if (&m == &error_metric) return error_metric;
    if (!ms->contains (c)) {
      array<rectangle> a;
      a << rectangle (m->x3, m->y3, m->x4, m->y4);
      rectangle t= eff->get_extents (a);
      metric_struct* r= tm_new<metric_struct> ();
      ms(c)= (pointer) r;
      r->x1= m->x1; r->y1= m->y1;
      r->x2= m->x2; r->y2= m->y2;
      r->x3= t->x1; r->y3= t->y1;
      r->x4= t->x2; r->y4= t->y2;
    }
    return *((metric*) ((void*) ms[c]));
  }
};

font_metric
effected (font_metric fnm, tree eff, SI em) {
  (void) em;
  string es= functional_to_string (eff);
  string name= "effected[" * fnm->res_name * "," * es * "]";
  effect e= build_effect (eff);
  return make (font_metric, name,
	       tm_new<effected_font_metric_rep> (name, fnm, e));
}

/******************************************************************************
* Glyphs as raster images
******************************************************************************/

raster<true_color>
as_raster (glyph gl) {
  int w= gl->width, h= gl->height;
  raster<true_color> ret (w, h, gl->xoff, h-1-gl->yoff);
  for (int y=0; y<h; y++)
    for (int x=0; x<w; x++)
      ret->a[w*(h-1-y)+x]= true_color (0.0, 0.0, 0.0, gl->get_x (x, y));
  return ret;
}

/******************************************************************************
* Raster images as glyphs
******************************************************************************/

static array<double>
get_thresholds () {
  array<int> a;
  a <<  1 << 14 << 18 << 04 << 16
    << 21 << 11 <<  6 << 23 <<  9
    <<  7 << 24 << 13 <<  2 << 19
    << 17 <<  3 << 20 << 15 <<  5
    << 10 << 22 <<  8 << 12 << 25;
  array<double> r;
  for (int i=0; i<25; i++)
    r << a[i] / 26.0;
  return r;
}

glyph
as_glyph (raster<true_color> r) {
  array<double> ths= get_thresholds ();
  int w= r->w, h= r->h;
  glyph bmr (w, h, r->ox, h-1-r->oy, 1);
  for (int y=0; y<h; y++)
    for (int x=0; x<w; x++) {
      true_color col= r->a[w*(h-1-y)+x];
      double th= ths[5*(y%5) + (x%5)];
      int val= (col.a < th? 0: 1);
      bmr->set_x (x, y, val);
    }
  return simplify (bmr);
}

/******************************************************************************
* Apply an effect to a glyph
******************************************************************************/

glyph
effected (glyph gl, effect eff) {
  array<picture> a;
  a << raster_picture (as_raster (gl));
  picture r= eff->apply (a, PIXEL);
  glyph bmr= as_glyph (as_raster<true_color> (r));
  bmr->lwidth= gl->lwidth;
  bmr->artistic= 1;
  return bmr;
}

/******************************************************************************
* Apply effect to a set of font glyphs
******************************************************************************/

struct effected_font_glyphs_rep: public font_glyphs_rep {
  font_glyphs fng;
  effect eff;
  hashmap<int,glyph> gs;
  effected_font_glyphs_rep (string name, font_glyphs fng2, effect e2):
    font_glyphs_rep (name), fng (fng2), eff (e2), gs (error_glyph) {}
  glyph& get (int c) {
    glyph& orig (fng->get (c));
    if ((&orig != &error_glyph) && !gs->contains (c))
      gs(c)= effected (orig, eff);
    return gs(c); }
};

font_glyphs
effected (font_glyphs fng, tree eff, SI em) {
  (void) em;
  string es= functional_to_string (eff);
  string name= "effected[" * fng->res_name * "," * es * "]";
  effect e= build_effect (eff);
  return make (font_glyphs, name,
               tm_new<effected_font_glyphs_rep> (name, fng, e));
}
