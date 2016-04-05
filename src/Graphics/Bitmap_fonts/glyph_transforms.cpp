
/******************************************************************************
* MODULE     : glyph_transforms.cpp
* DESCRIPTION: transformations on glyphs needed for global font modulations
* COPYRIGHT  : (C) 2016  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "bitmap_font.hpp"
#include "renderer.hpp"

extern glyph error_glyph;
extern metric error_metric;

/******************************************************************************
* Slanted font metrics
******************************************************************************/

struct slanted_font_metric_rep: public font_metric_rep {
  font_metric fnm;
  double slant;
  hashmap<int,pointer> ms;
  slanted_font_metric_rep (string name, font_metric fnm2, double slant2):
    font_metric_rep (name), fnm (fnm2), slant (slant2), ms (error_metric) {}
  bool exists (int c) { return fnm->exists (c); }
  metric& get (int c) {
    metric& m (fnm->get (c));
    if (&m == &error_metric) return error_metric;
    if (!ms->contains (c)) {
      metric_struct* r= tm_new<metric_struct> ();
      ms(c)= (pointer) r;
      r->x1= m->x1;
      r->x2= m->x2;
      r->x3= m->x3 + (SI) floor (m->y3 * slant);
      r->x4= m->x4 + (SI) ceil  (m->y4 * slant);
      r->y1= m->y1;
      r->y2= m->y2;
      r->y3= m->y3;
      r->y4= m->y4;
    }
    return *((metric*) ((void*) ms[c])); }
};

font_metric
slanted (font_metric fnm, double slant) {
  string name= fnm->res_name * "-slanted[" * as_string (slant) * "]";
  return make (font_metric, name,
	       tm_new<slanted_font_metric_rep> (name, fnm, slant));
}

/******************************************************************************
* Slanted font glyphs
******************************************************************************/

glyph
slanted (glyph gl, double slant) {
  int i, j;
  int ww= gl->width, hh= gl->height;
  int t= gl->yoff, b= (hh-1) - gl->yoff;
  int l= (int) ceil (slant * b), r= (int) ceil (slant * t);
  int nw= ww + l + r;
  glyph bmr (nw, hh, gl->xoff + l, gl->yoff, gl->depth);
  for (j=0; j<hh; j++) {
    int dx= (int) (floor (slant * (gl->yoff - j) + 0.5));
    for (i=0; i<nw; i++)
      bmr->set_x (i, j, 0);
    for (i=0; i<ww; i++)
      bmr->set_x (i+l+dx, j, gl->get_x (i, j));
  }
  return bmr;
}

struct slanted_font_glyphs_rep: public font_glyphs_rep {
  font_glyphs fng;
  double slant;
  hashmap<int,glyph> gs;
  slanted_font_glyphs_rep (string name, font_glyphs fng2, double slant2):
    font_glyphs_rep (name), fng (fng2), slant (slant2), gs (error_glyph) {}
  glyph& get (int c) {
    glyph& orig (fng->get (c));
    if ((&orig != &error_glyph) && !gs->contains (c))
      gs(c)= slanted (orig, slant);
    return gs(c); }
};

font_glyphs
slanted (font_glyphs fng, double slant) {
  string name= fng->res_name * "-slanted[" * as_string (slant) * "]";
  return make (font_glyphs, name,
               tm_new<slanted_font_glyphs_rep> (name, fng, slant));
}
