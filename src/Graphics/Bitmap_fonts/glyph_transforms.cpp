
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
#include "hashset.hpp"

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

/******************************************************************************
* Boldening of font metrics
******************************************************************************/

struct bolden_font_metric_rep: public font_metric_rep {
  font_metric fnm;
  SI dtot;
  hashmap<int,pointer> ms;
  bolden_font_metric_rep (string name, font_metric fnm2, SI dtot2):
    font_metric_rep (name), fnm (fnm2), dtot (dtot2), ms (error_metric) {}
  bool exists (int c) { return fnm->exists (c); }
  metric& get (int c) {
    metric& m (fnm->get (c));
    if (&m == &error_metric) return error_metric;
    if (!ms->contains (c)) {
      metric_struct* r= tm_new<metric_struct> ();
      ms(c)= (pointer) r;
      r->x1= m->x1;
      r->x2= m->x2 + dtot;
      r->x3= m->x3;
      r->x4= m->x4 + dtot;
      r->y1= m->y1;
      r->y2= m->y2;
      r->y3= m->y3;
      r->y4= m->y4;
    }
    return *((metric*) ((void*) ms[c])); }
};

font_metric
bolden (font_metric fnm, SI dtot) {
  string name= fnm->res_name * "-bolden[" * as_string (dtot) * "]";
  return make (font_metric, name,
	       tm_new<bolden_font_metric_rep> (name, fnm, dtot));
}

/******************************************************************************
* Boldening of font glyphs
******************************************************************************/

glyph
bolden (glyph gl, SI dpen) {
  int dw= (dpen + (PIXEL/2)) / PIXEL;
  int i, j;
  int ww= gl->width, hh= gl->height;
  glyph bmr (ww + dw, hh, gl->xoff, gl->yoff, gl->depth);
  for (j=0; j<hh; j++)
    for (i=0; i<(ww+dw); i++)
      bmr->set_x (i, j, 0);
  for (j=0; j<hh; j++)
    for (i=0; i<ww; i++) {
      int val= gl->get_x (i, j);
      for (int k=0; k<=dw; k++)
        bmr->set_x (i+k, j, max (val, bmr->get_x (i+k, j)));
    }
  return bmr;
}

struct bolden_font_glyphs_rep: public font_glyphs_rep {
  font_glyphs fng;
  SI dpen;
  hashmap<int,glyph> gs;
  bolden_font_glyphs_rep (string name, font_glyphs fng2, SI dpen2):
    font_glyphs_rep (name), fng (fng2), dpen (dpen2), gs (error_glyph) {}
  glyph& get (int c) {
    glyph& orig (fng->get (c));
    if ((&orig != &error_glyph) && !gs->contains (c))
      gs(c)= bolden (orig, dpen);
    return gs(c); }
};

font_glyphs
bolden (font_glyphs fng, SI dpen) {
  string name= fng->res_name * "-bolden[" * as_string (dpen) * "]";
  return make (font_glyphs, name,
               tm_new<bolden_font_glyphs_rep> (name, fng, dpen));
}

/******************************************************************************
* Detailed analysis for how to emulate blackboard bold fonts
******************************************************************************/

void
get_line_offsets (glyph gl, int j, int& start, int& end) {
  int ww= gl->width;
  for (int i=0; i<ww; i++)
    if (gl->get_x (i, j) != 0) {
      start= i;
      for (; i<ww; i++)
	if (i+1 >= ww || gl->get_x (i+1, j) == 0) {
	  end= i;
	  return;
	}
    }
  start= end= -1;
}

void
adjust_bbb_offsets (glyph gl, SI delta, int next, int prev,
		    array<int>& start, array<int>& end) {
  int ww= gl->width;
  if (start[prev] == -1) {
    start[next]= end[next]= -1;
    return;
  }

  if (end[next] + delta < start[prev]) {
    int lo= start[prev] - delta;
    int hi= min (end[prev] + delta + 1, ww);
    for (int i= lo; i < hi; i++)
      if (gl->get_x (i, next) != 0) {
	int i1= i;
	while (i1 > 0 && gl->get_x (i1-1, next) != 0) i1--;
	int i2= i;
	while (i2+1 < ww && gl->get_x (i2+1, next) != 0) i2--;
	start[next]= i1;
	end  [next]= i2;
	return;
      }
    start[next]= end[next]= -1;
    return;    
  }

  if (start[next] > end[prev] + delta) {
    start[next]= end[next]= -1;
    return;
  }
}

void
get_bbb_offsets (glyph gl, SI fat, array<int>& start, array<int>& end) {
  int dw= (fat + (PIXEL/2)) / PIXEL;
  int hh= gl->height;
  start= array<int> (hh);
  end  = array<int> (hh);
  for (int j=0; j<hh; j++)
    get_line_offsets (gl, j, start[j], end[j]);
  for (int j=0; j<hh; j++)
    if (start[j] != -1) {
      int j1= j;
      while (j+1<hh && start[j+1] != -1) j++;
      int j2= j;
      int m = (j1 + j2) >> 1;
      int delta= max (1, dw/4);
      for (int k=m+1; k<=j2; k++)
	adjust_bbb_offsets (gl, delta, k, k-1, start, end);
      for (int k=m-1; k>=j1; k--)
	adjust_bbb_offsets (gl, delta, k, k+1, start, end);
    }
}

glyph
bolden_at (glyph gl, array<int> start, SI pen, SI fat) {
  int dw= (fat + (PIXEL/2)) / PIXEL;
  int i, j;
  int ww= gl->width, hh= gl->height;
  glyph bmr (ww + dw, hh, gl->xoff, gl->yoff, gl->depth);
  for (j=0; j<hh; j++) {
    int off= start[j];
    for (i=0; i<ww; i++)
      if (i < off) bmr->set_x (i, j, gl->get_x (i, j));
      else if (i > off) bmr->set_x (i+dw, j, gl->get_x (i, j));
      else for (int k=0; k<=dw; k++)
	     bmr->set_x (i+k, j, 1);
  }
  return bmr;
}

glyph
hollow (glyph gl, array<int> start, array<int> end, SI pen, SI fat) {
  int dw= (fat + (PIXEL/2)) / PIXEL;
  int r= (pen + (PIXEL/2)) / PIXEL;
  int i, j;
  int ww= gl->width, hh= gl->height;
  glyph bmr (ww, hh, gl->xoff, gl->yoff, gl->depth);
  for (j=0; j<hh; j++) {
    int i1= start[j];
    int i2= start[j] + dw + (r >> 1);
    //int i2= end  [j] + dw;
    for (i=0; i<ww; i++) {
      bmr->set_x (i, j, gl->get_x (i, j));
      if (i > i1 && i2 > i) {
	bool erase= ((i>=r) && (j>=r) && (i+r<ww) && (hh>j+r));
        if (erase) {
	  int c0= 0, c1= 0;
	  for (int dj= -r; dj <= r; dj++)
	    for (int di= -r; di <= r; di++)
	      if ((di*di + dj*dj) <= r*r)
		if (gl->get_x (i+di, j+dj) == 0) c0++;
		else c1++;
	  erase= 20*c0 < c1;
	}
	if (erase) bmr->set_x (i, j, 0);
      }
    }
  }
  return bmr;
}

glyph
var_make_bbb (glyph gl, SI pen, SI fat) {
  array<int> start, end;
  get_bbb_offsets (gl, fat, start, end);
  glyph bgl= bolden_at (gl, start, pen, fat);
  return hollow (bgl, start, end, pen, fat);
}

/******************************************************************************
* Emulation of blackboard bold fonts
******************************************************************************/

glyph
hollow (glyph gl, SI pen) {
  int r= (pen + (PIXEL/2)) / PIXEL;
  int i, j;
  int ww= gl->width, hh= gl->height;
  glyph bmr (ww, hh, gl->xoff, gl->yoff, gl->depth);
  for (j=0; j<hh; j++)
    for (i=0; i<ww; i++) {
      bmr->set_x (i, j, gl->get_x (i, j));
      bool erase= i>=r && j>=r && ww>i+r && hh>j+r;
      if (erase) {
	int c0= 0, c1= 0;
        for (int dj= -r; dj <= r; dj++)
          for (int di= -r; di <= r; di++)
            if ((di*di + dj*dj) <= r*r)
              if (gl->get_x (i+di, j+dj) == 0) c0++;
	      else c1++;
	erase= 20*c0 < c1;
      }
      if (erase) bmr->set_x (i, j, 0);
    }
  return bmr;
}

glyph
make_bbb (glyph gl, SI pen, SI fat) {
  int dw= (fat + (PIXEL/2)) / PIXEL;
  int i, j;
  int ww= gl->width, hh= gl->height;
  glyph bmr (ww + dw, hh, gl->xoff, gl->yoff, gl->depth);
  for (j=0; j<hh; j++) {    
    for (i=0; i<dw; i++)
      bmr->set_x (i, j, 0);
    for (i=0; i<ww; i++)
      bmr->set_x (i+dw, j, gl->get_x (i, j));
  }
  for (j=0; j<hh; j++)
    for (i=0; i<ww; i++)
      if (gl->get_x (i, j) != 0) {
        for (int k=0; k<dw; k++)
          bmr->set_x (i+k, j, 1);
        break;
      }
  return hollow (bmr, pen);
}

static hashset<int> bbb_left;
static hashset<int> bbb_right;

static void
bbb_initialize () {
  if (N(bbb_left) != 0) return;
  bbb_left  << ((int) 'K')<< ((int) 'N') << ((int) 'R');
  bbb_right << ((int) '1') << ((int) '2') << ((int) '3')
            << ((int) '5') << ((int) '7') << ((int) '9')
            << ((int) 'J')
            << ((int) 'a') << ((int) 'd') << ((int) 'g') << ((int) 'j')
            << ((int) 'q') << ((int) 'y')
            << ((int) ')') << ((int) ']') << ((int) '}');
}

glyph
make_bbb (glyph gl, int code, SI pen, SI fat) {
  bbb_initialize ();
  if (bbb_right->contains (code)) {
    glyph fgl = hor_flip (gl);
    glyph fret= var_make_bbb (fgl, pen, fat);
    return hor_flip (fret);
  }
  else if (true || bbb_left->contains (code))
    return var_make_bbb (gl, pen, fat);
  else if (bbb_left->contains (code))
    return make_bbb (gl, pen, fat);
  else if (false && bbb_right->contains (code)) {
    glyph fgl = hor_flip (gl);
    glyph fret= make_bbb (fgl, pen, fat);
    return hor_flip (fret);
  }
  else return hollow (bolden (gl, fat), pen);
}

struct make_bbb_font_glyphs_rep: public font_glyphs_rep {
  font_glyphs fng;
  SI penw, fatw;
  hashmap<int,glyph> gs;
  make_bbb_font_glyphs_rep (string name, font_glyphs fng2, SI pw, SI fw):
    font_glyphs_rep (name), fng (fng2),
    penw (pw), fatw (fw), gs (error_glyph) {}
  glyph& get (int c) {
    glyph& orig (fng->get (c));
    if ((&orig != &error_glyph) && !gs->contains (c))
      gs(c)= make_bbb (orig, c, penw, fatw);
    return gs(c); }
};

font_glyphs
make_bbb (font_glyphs fng, SI penw, SI fatw) {
  string name= fng->res_name * "-make_bbb[" * as_string (penw);
  if (fatw != 4 * penw) name << "," << as_string (fatw);
  name << "]";
  return make (font_glyphs, name,
               tm_new<make_bbb_font_glyphs_rep> (name, fng, penw, fatw));
}
