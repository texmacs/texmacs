
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
* Stretching of font metrics
******************************************************************************/

struct stretched_font_metric_rep: public font_metric_rep {
  font_metric fnm;
  double xf, yf;
  hashmap<int,pointer> ms;
  stretched_font_metric_rep (string name, font_metric fnm2,
                             double xf2, double yf2):
    font_metric_rep (name), fnm (fnm2),
    xf (xf2), yf (yf2), ms (error_metric) {}
  bool exists (int c) { return fnm->exists (c); }
  metric& get (int c) {
    metric& m (fnm->get (c));
    if (&m == &error_metric) return error_metric;
    if (!ms->contains (c)) {
      metric_struct* r= tm_new<metric_struct> ();
      ms(c)= (pointer) r;
      r->x1= (SI) floor (xf * m->x1 + 0.5);
      r->x2= (SI) floor (xf * m->x2 + 0.5);
      r->x3= (SI) floor (xf * m->x3);
      r->x4= (SI) ceil  (xf * m->x4);
      r->y1= (SI) floor (yf * m->y1 + 0.5);
      r->y2= (SI) floor (yf * m->y2 + 0.5);
      r->y3= (SI) floor (yf * m->y3);
      r->y4= (SI) ceil  (yf * m->y4);
    }
    return *((metric*) ((void*) ms[c])); }
};

font_metric
stretched (font_metric fnm, double xf, double yf) {
  string name= fnm->res_name * "-stretched[";
  name << as_string (xf) << "," << as_string (yf) << "]";
  return make (font_metric, name,
	       tm_new<stretched_font_metric_rep> (name, fnm, xf, yf));
}

/******************************************************************************
* Vertical stretching of font glyphs
******************************************************************************/

glyph
stretched (glyph gl, double xf, double yf) {
  int i, j, I, J;
  int ww= gl->width, hh= gl->height;
  int x1= -gl->xoff, x2= ww - gl->xoff;
  int X1= (int) floor (xf * x1), X2= (int) ceil (xf * x2);
  int WW= X2 - X1;
  int y1= gl->yoff - hh, y2= gl->yoff;
  int Y1= (int) floor (yf * y1), Y2= (int) ceil (yf * y2);
  int HH= Y2 - Y1;
  glyph bmr (WW, HH, -X1, Y2, gl->depth);
  for (I=0; I<WW; I++) {
    int X = I + X1;
    int i1= ((int) floor (X / xf)) - x1;
    int i2= ((int) ceil  ((X + 1) / xf)) - x1;
    i1= max (min (i1, ww-1), 0);
    i2= max (min (i2, ww-1), 0);
    for (J=0; J<HH; J++) {
      int Y = J + Y1;
      int j1= ((int) floor (Y / yf)) - y1;
      int j2= ((int) ceil  ((Y + 1) / yf)) - y1;
      j1= max (min (j1, hh-1), 0);
      j2= max (min (j2, hh-1), 0);
      double sum= 0.0;
      for (i= i1; i<i2; i++)
        for (j= j1; j<j2; j++)
          if (gl->get_x (i, j)) {
            double X1b= xf * (i + x1);
            double X2b= xf * ((i + 1) + x1);
            X1b= max (X + 0.0, X1b);
            X2b= min (X + 1.0, X2b);
            double Y1b= yf * (j + y1);
            double Y2b= yf * ((j + 1) + y1);
            Y1b= max (Y + 0.0, Y1b);
            Y2b= min (Y + 1.0, Y2b);
            if (X1b < X2b && Y1b < Y2b)
              sum += (X2b - X1b) * (Y2b - Y1b);
          }
      bmr->set_x (I, J, sum >= 0.5? 1: 0);
    }
  }
  return bmr;
}

struct stretched_font_glyphs_rep: public font_glyphs_rep {
  font_glyphs fng;
  double xf, yf;
  hashmap<int,glyph> gs;
  stretched_font_glyphs_rep (string name, font_glyphs fng2,
                             double xf2, double yf2):
    font_glyphs_rep (name), fng (fng2),
    xf (xf2), yf (yf2), gs (error_glyph) {}
  glyph& get (int c) {
    glyph& orig (fng->get (c));
    if ((&orig != &error_glyph) && !gs->contains (c))
      gs(c)= stretched (orig, xf, yf);
    return gs(c); }
};

font_glyphs
stretched (font_glyphs fng, double xf, double yf) {
  string name= fng->res_name * "-stretched[";
  name << as_string (xf) << "," << as_string (yf) << "]";
  return make (font_glyphs, name,
               tm_new<stretched_font_glyphs_rep> (name, fng, xf, yf));
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

glyph
bolden (glyph gl, SI dpen, SI dtot) {
  if (dtot <= dpen) return bolden (gl, dpen);
  SI xw= gl->width * PIXEL;
  double c= 1.0; // approximation of orig_penw / dpen
  double eps= (c * (dtot - dpen)) / (xw + c * (dtot - dpen));
  SI dpen2= (SI) ((1.0 - eps) * dpen);
  double lambda= ((double) (xw + dtot - dpen2)) / ((double) xw);
  glyph wgl= stretched (gl, lambda, 1.0);
  return bolden (wgl, dpen2);
}

struct bolden_font_glyphs_rep: public font_glyphs_rep {
  font_glyphs fng;
  SI dpen, dtot;
  hashmap<int,glyph> gs;
  bolden_font_glyphs_rep (string name, font_glyphs fng2, SI dpen2, SI dtot2):
    font_glyphs_rep (name), fng (fng2),
    dpen (dpen2), dtot (dtot2), gs (error_glyph) {}
  glyph& get (int c) {
    glyph& orig (fng->get (c));
    if ((&orig != &error_glyph) && !gs->contains (c))
      gs(c)= bolden (orig, dpen, dtot);
    return gs(c); }
};

font_glyphs
bolden (font_glyphs fng, SI dpen, SI dtot) {
  string name= fng->res_name * "-bolden[" * as_string (dpen);
  if (dtot != dpen) name << "," << as_string (dtot) << "]";
  return make (font_glyphs, name,
               tm_new<bolden_font_glyphs_rep> (name, fng, dpen, dtot));
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

int
left_width (glyph gl) {
  int hh= gl->height;
  int s1, e1, s2, e2, s3, e3;
  get_line_offsets (gl, (4*hh)/10, s1, e1);
  get_line_offsets (gl, (5*hh)/10, s2, e2);
  get_line_offsets (gl, (6*hh)/10, s3, e3);
  return min (e1-s1+1, min (e2-s2+1, e3-s3+1));
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
bolden_at (glyph gl, array<int> start, SI fat) {
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
hollow (glyph gl, array<int> start, SI penw, SI penh, SI fat) {
  double rx= ((double) penw) / ((double) PIXEL);
  double ry= ((double) penh) / ((double) PIXEL);
  int    Rx= (int) floor (rx + 0.5);
  int    Ry= (int) floor (ry + 0.5);
  int dw= (fat + (PIXEL/2)) / PIXEL;
  int i, j;
  int ww= gl->width, hh= gl->height;
  glyph bmr (ww, hh, gl->xoff, gl->yoff, gl->depth);
  for (j=0; j<hh; j++) {
    int i1= start[j];
    int i2= start[j] + dw; // + (Rx >> 1);
    for (i=0; i<ww; i++) {
      bmr->set_x (i, j, gl->get_x (i, j));
      if (i > i1 && i2 > i) {
	bool erase= ((i>=Rx) && (j>=Ry) && (i+Rx<ww) && (hh>j+Ry));
        if (erase) {
	  int c0= 0, c1= 0;
	  for (int dj= -Ry; dj <= Ry; dj++)
	    for (int di= -Rx; di <= Rx; di++) {
	      double fx= di/rx, fy= dj/ry;
	      if ((fx*fx + fy*fy) <= 1.0) {
                //if (max (abs (fx), abs (fy)) <= 1.0) {
                //if (abs (fx) + abs (fy) <= 1.0) {
		if (gl->get_x (i+di, j+dj) == 0) c0++;
		else c1++;
	      }
	    }
	  erase= 20*c0 < c1;
	}
	if (erase) bmr->set_x (i, j, 0);
      }
    }
  }
  return bmr;
}

static void
fix_left_border (glyph gl, array<int>& start, SI& penw, SI& fat) {
  SI lw= left_width (gl) * PIXEL;
  if (4*lw < 3*penw) {
    if (true) {
      fat  -= ((penw >> 1) - lw);
      penw -= ((penw - lw) >> 1);
    }
    else if (true) {
      fat -= (penw - 2*lw);
      penw = lw;
    }
    else if (true) {
      for (int j=0; j<N(start); j++)
        if (start[j] >= 0)
          start[j] += (penw - lw) / PIXEL;
      fat -= (penw - lw);
      penw = lw;
    }
  }
}

glyph
var_make_bbb (glyph gl, int code, SI penw, SI penh, SI fat) {
  array<int> start, end;
  get_bbb_offsets (gl, fat, start, end);
  glyph bgl= bolden_at (gl, start, fat);
  if (code == ((int) 'A') || code == ((int) 'M') || code == ((int) 'N'))
    fix_left_border (gl, start, penw, fat);
  return hollow (bgl, start, penw, penh, fat);
}

/******************************************************************************
* Emulation of blackboard bold fonts
******************************************************************************/

glyph
hollow (glyph gl, SI penw, SI penh) {
  double rx= ((double) penw) / ((double) PIXEL);
  double ry= ((double) penh) / ((double) PIXEL);
  int    Rx= (int) floor (rx + 0.5);
  int    Ry= (int) floor (ry + 0.5);
  int i, j;
  int ww= gl->width, hh= gl->height;
  glyph bmr (ww, hh, gl->xoff, gl->yoff, gl->depth);
  for (j=0; j<hh; j++)
    for (i=0; i<ww; i++) {
      bmr->set_x (i, j, gl->get_x (i, j));
      bool erase= i>=Rx && j>=Ry && ww>i+Rx && hh>j+Ry;
      if (erase) {
	int c0= 0, c1= 0;
        for (int dj= -Ry; dj <= Ry; dj++)
          for (int di= -Rx; di <= Rx; di++) {
	    double fx= di/rx, fy= dj/ry;
            if ((fx*fx + fy*fy) <= 1.0)
              if (gl->get_x (i+di, j+dj) == 0) c0++;
	      else c1++;
	  }
	erase= 20*c0 < c1;
      }
      if (erase) bmr->set_x (i, j, 0);
    }
  return bmr;
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
    //      << ((int) 'A') << ((int) 'J') << ((int) 'M')
            << ((int) 'a') << ((int) 'd') << ((int) 'g') << ((int) 'j')
            << ((int) 'q') << ((int) 'y')
            << ((int) ')') << ((int) ']') << ((int) '}');
}

glyph
make_bbb (glyph gl, int code, SI penw, SI penh, SI fat) {
  bbb_initialize ();
  if (bbb_right->contains (code)) {
    glyph fgl = hor_flip (gl);
    glyph fret= var_make_bbb (fgl, code, penw, penh, fat);
    return hor_flip (fret);
  }
  else if (true || bbb_left->contains (code))
    return var_make_bbb (gl, code, penw, penh, fat);
  else return hollow (bolden (gl, fat), penw, penh);
}

struct make_bbb_font_glyphs_rep: public font_glyphs_rep {
  font_glyphs fng;
  SI penw, penh, fatw;
  hashmap<int,glyph> gs;
  make_bbb_font_glyphs_rep (string name, font_glyphs fng2,
			    SI pw, SI ph, SI fw):
    font_glyphs_rep (name), fng (fng2),
    penw (pw), penh (ph), fatw (fw), gs (error_glyph) {}
  glyph& get (int c) {
    glyph& orig (fng->get (c));
    if ((&orig != &error_glyph) && !gs->contains (c))
      gs(c)= make_bbb (orig, c, penw, penh, fatw);
    return gs(c); }
};

font_glyphs
make_bbb (font_glyphs fng, SI penw, SI penh, SI fatw) {
  string name= fng->res_name * "-make_bbb[" * as_string (penw);
  name << "," << as_string (penh);
  name << "," << as_string (fatw) << "]";
  return make (font_glyphs, name,
               tm_new<make_bbb_font_glyphs_rep> (name, fng, penw, penh, fatw));
}
