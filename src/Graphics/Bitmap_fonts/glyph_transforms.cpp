
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

/******************************************************************************
* Vertical stretching of font metrics
******************************************************************************/

struct vstretch_font_metric_rep: public font_metric_rep {
  font_metric fnm;
  double factor;
  hashmap<int,pointer> ms;
  vstretch_font_metric_rep (string name, font_metric fnm2, double factor2):
    font_metric_rep (name), fnm (fnm2), factor (factor2), ms (error_metric) {}
  bool exists (int c) { return fnm->exists (c); }
  metric& get (int c) {
    metric& m (fnm->get (c));
    if (&m == &error_metric) return error_metric;
    if (!ms->contains (c)) {
      metric_struct* r= tm_new<metric_struct> ();
      ms(c)= (pointer) r;
      r->x1= m->x1;
      r->x2= m->x2;
      r->x3= m->x3;
      r->x4= m->x4;
      r->y1= (SI) floor (factor * m->y1 + 0.5);
      r->y2= (SI) floor (factor * m->y2 + 0.5);
      r->y3= (SI) floor (factor * m->y3);
      r->y4= (SI) ceil  (factor * m->y4);
    }
    return *((metric*) ((void*) ms[c])); }
};

font_metric
vstretch (font_metric fnm, double factor) {
  string name= fnm->res_name * "-vstretch[" * as_string (factor) * "]";
  return make (font_metric, name,
	       tm_new<vstretch_font_metric_rep> (name, fnm, factor));
}

/******************************************************************************
* Vertical stretching of font glyphs
******************************************************************************/

glyph
vstretch (glyph gl, double factor) {
  int i, j, J;
  int ww= gl->width, hh= gl->height;
  int y1= gl->yoff - hh, y2= gl->yoff;
  int Y1= (int) floor (factor * y1), Y2= (int) ceil (factor * y2);
  int HH= Y2 - Y1;
  glyph bmr (ww, HH, gl->xoff, Y2, gl->depth);
  for (i=0; i<ww; i++)
    for (J=0; J<HH; J++) {
      int Y = J + Y1;
      int j1= ((int) floor (Y / factor)) - y1;
      int j2= ((int) ceil  ((Y + 1) / factor)) - y1;
      j1= max (min (j1, hh-1), 0);
      j2= max (min (j2, hh-1), 0);
      double sum= 0.0;
      for (j= j1; j<j2; j++)
        if (gl->get_x (i, j)) {
          double Y1b= factor * (j + y1);
          double Y2b= factor * ((j + 1) + y1);
          Y1b= max (Y + 0.0, Y1b);
          Y2b= min (Y + 1.0, Y2b);
          if (Y1b < Y2b) sum += Y2b - Y1b;
        }
      bmr->set_x (i, J, sum >= 0.5? 1: 0);
    }
  return bmr;
}

struct vstretch_font_glyphs_rep: public font_glyphs_rep {
  font_glyphs fng;
  double factor;
  hashmap<int,glyph> gs;
  vstretch_font_glyphs_rep (string name, font_glyphs fng2, double factor2):
    font_glyphs_rep (name), fng (fng2), factor (factor2), gs (error_glyph) {}
  glyph& get (int c) {
    glyph& orig (fng->get (c));
    if ((&orig != &error_glyph) && !gs->contains (c))
      gs(c)= vstretch (orig, factor);
    return gs(c); }
};

font_glyphs
vstretch (font_glyphs fng, double factor) {
  string name= fng->res_name * "-vstretch[" * as_string (factor) * "]";
  return make (font_glyphs, name,
               tm_new<vstretch_font_glyphs_rep> (name, fng, factor));
}
