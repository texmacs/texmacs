
/******************************************************************************
* MODULE     : virtual_font.cpp
* DESCRIPTION: fonts consisting of extra symbols which can be generated
*              automatically from a defining tree
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "font.hpp"
#include "translator.hpp"
#include "analyze.hpp"
#include "frame.hpp"
#include "iterator.hpp"

int get_utf8_code (string c);

/******************************************************************************
* The virtual font class
******************************************************************************/

struct virtual_font_rep: font_rep {
  font         base_fn;
  string       fn_name;
  translator   virt;
  int          size;
  int          hdpi;
  int          vdpi;
  bool         extend;
  int          last;
  font_metric  fnm;
  font_glyphs  fng;
  double       hunit;
  double       vunit;
  hashmap<scheme_tree,metric_struct> trm;
  hashmap<scheme_tree,glyph> trg;
  hashmap<string,bool> sup_bit;
  hashmap<string,bool> sup_svg;

  virtual_font_rep (string name, font base, string vname, int size,
                    int hdpi, int vdpi, bool extend);
  scheme_tree exec (scheme_tree t);
  bool   supported (scheme_tree t, bool svg);
  bool   supported (string c, bool svg);
  glyph  compile_bis (scheme_tree t, metric& ex);
  glyph  compile (scheme_tree t, metric& ex);
  void   get_metric (scheme_tree t, metric& ex);
  tree   get_tree (string s);
  void   draw_tree (renderer ren, scheme_tree t, SI x, SI y);
  void   draw_clipped (renderer ren, scheme_tree t, SI x, SI y,
                       SI x1, SI y1, SI x2, SI y2);
  void   draw_transformed (renderer ren, scheme_tree t, SI x, SI y, frame f);
  void   advance_glyph (string s, int& pos, bool ligf);
  int    get_char (string s, font_metric& fnm, font_glyphs& fng);
  glyph  get_glyph (string s);
  int    index_glyph (string s, font_metric& fnm, font_glyphs& fng);

  bool   supports (string c);
  void   get_extents (string s, metric& ex);
  void   get_xpositions (string s, SI* xpos);
  void   get_xpositions (string s, SI* xpos, bool lit);
  void   get_xpositions (string s, SI* xpos, SI xk);
  void   draw_fixed (renderer ren, string s, SI x, SI y);
  void   draw_fixed (renderer ren, string s, SI x, SI y, SI xk);
  font   magnify (double zoomx, double zoomy);

  double get_left_slope (string s);
  double get_right_slope (string s);
  SI     get_right_correction (string s);
  SI     get_lsub_correction  (string s);
  SI     get_lsup_correction  (string s);
  SI     get_rsub_correction  (string s);
  SI     get_rsup_correction  (string s);
  SI     get_wide_correction  (string s, int mode);
};

virtual_font_rep::virtual_font_rep (
  string name, font base, string vname, int size2,
  int hdpi2, int vdpi2, bool extend2):
    font_rep (name, base), base_fn (base), fn_name (vname),
    virt (load_translator (vname)), size (size2),
    hdpi (hdpi2), vdpi (vdpi2), extend (extend2),
    last (N(virt->virt_def)),
    fnm (std_font_metric (name, tm_new_array<metric> (last), 0, last-1)),
    fng (std_font_glyphs (name, tm_new_array<glyph> (last), 0, last-1)),
    trm (metric_struct ()), trg (glyph ()),
    sup_bit (false), sup_svg (false)
{
  copy_math_pars (base_fn);
  hunit= ((size*hdpi)/72)*PIXEL;
  vunit= ((size*vdpi)/72)*PIXEL;
}

/******************************************************************************
* Execution of expressions
******************************************************************************/

scheme_tree
virtual_font_rep::exec (scheme_tree t) {
  //cout << "Exec " << t << "\n";
  if (is_atomic (t)) return t;
  else if (is_tuple (t, "+")) {
    scheme_tree r= "0";
    for (int i=1; i<N(t); i++) {
      scheme_tree a= exec (t[i]);
      if (!is_double (a)) return "error";
      r= as_string (as_double (r) + as_double (a));
    }
    return r;
  }
  else if (is_tuple (t, "-", 2)) {
    scheme_tree a= exec (t[1]);
    scheme_tree b= exec (t[2]);
    if (!(is_double (a) && is_double (b))) return "error";
    return as_string (as_double (a) - as_double (b));
  }
  else if (is_tuple (t, "*")) {
    scheme_tree r= "1";
    for (int i=1; i<N(t); i++) {
      scheme_tree a= exec (t[i]);
      if (!is_double (a)) return "error";
      r= as_string (as_double (r) * as_double (a));
    }
    return r;
  }
  else if (is_tuple (t, "/", 2)) {
    scheme_tree a= exec (t[1]);
    scheme_tree b= exec (t[2]);
    if (!(is_double (a) && is_double (b))) return "error";
    return as_string (as_double (a) / as_double (b));
  }
  else if (is_tuple (t, "min")) {
    scheme_tree r= "1000000000.0";
    for (int i=1; i<N(t); i++) {
      scheme_tree a= exec (t[i]);
      if (!is_double (a)) return "error";
      r= as_string (min (as_double (r), as_double (a)));
    }
    return r;
  }
  else if (is_tuple (t, "max")) {
    scheme_tree r= "-1000000000.0";
    for (int i=1; i<N(t); i++) {
      scheme_tree a= exec (t[i]);
      if (!is_double (a)) return "error";
      r= as_string (max (as_double (r), as_double (a)));
    }
    return r;
  }
  else if (is_tuple (t, "width", 1)) {
    metric ex;
    get_metric (t[1], ex);
    double w= ((double) (ex->x2 - ex->x1)) / hunit;
    return as_string (w);
  }
  else if (is_tuple (t, "height", 1)) {
    metric ex;
    get_metric (t[1], ex);
    double w= ((double) (ex->y2 - ex->y1)) / hunit;
    return as_string (w);
  }
  else if (is_tuple (t, "xpos", 2) || is_tuple (t, "ypos", 2) ||
           is_tuple (t, "xpos", 4) || is_tuple (t, "ypos", 4)) {
    metric ex;
    glyph  gl= compile (t[1], ex);
    double xf= 0.5;
    double yf= 0.5;
    if (is_tuple (t, "xpos", 2) && is_double (t[2])) xf= as_double (t[2]);
    if (is_tuple (t, "ypos", 2) && is_double (t[2])) yf= as_double (t[2]);
    if (N(t) == 5 && is_double (t[2])) xf= as_double (t[2]);
    if (N(t) == 5 && is_double (t[3])) yf= as_double (t[3]);
    SI x= (SI) (ex->x1 + xf * (ex->x2 - ex->x1));
    SI y= (SI) (ex->y1 + yf * (ex->y2 - ex->y1));
    SI dx= 0, dy= 0;
    int xx= (int) floor (((double) x) / PIXEL + 0.5);
    int yy= (int) floor (((double) y) / PIXEL + 0.5);
    if (is_tuple (t, "xpos", 4) && t[4] == "+")
      dx= probe (gl, xx, yy,  1,  0) * PIXEL;
    if (is_tuple (t, "xpos", 4) && t[4] == "-")
      dx= probe (gl, xx, yy, -1,  0) * PIXEL;
    if (is_tuple (t, "ypos", 4) && t[4] == "+")
      dy= probe (gl, xx, yy,  0,  1) * PIXEL;
    if (is_tuple (t, "ypos", 4) && t[4] == "-")
      dy= probe (gl, xx, yy,  0, -1) * PIXEL;
    double r;
    if (is_tuple (t, "xpos"))
      r= ((double) (x+dx)) / hunit;
    else
      r= ((double) (y+dy)) / vunit;
    return as_string (r);
  }
  else if (is_tuple (t, "penw", 4) || is_tuple (t, "penh", 4)) {
    metric ex;
    glyph  gl= compile (t[1], ex);
    double x1f= 0.0, x2f= 1.0;
    double y1f= 0.0, y2f= 1.0;
    if (is_double (t[2])) x1f= x2f= as_double (t[2]);
    if (is_double (t[4])) y1f= y2f= as_double (t[4]);
    if (is_tuple (t, "penw") && is_double (t[3])) x2f= as_double (t[3]);
    if (is_tuple (t, "penh") && is_double (t[3])) y1f= as_double (t[3]);
    SI x1= (SI) (ex->x1 + x1f * (ex->x2 - ex->x1));
    SI x2= (SI) (ex->x1 + x2f * (ex->x2 - ex->x1));
    SI y1= (SI) (ex->y1 + y1f * (ex->y2 - ex->y1));
    SI y2= (SI) (ex->y1 + y2f * (ex->y2 - ex->y1));
    int xx1= (int) floor (((double) x1) / PIXEL + 0.5);
    int xx2= (int) floor (((double) x2) / PIXEL + 0.5);
    int yy1= (int) floor (((double) y1) / PIXEL + 0.5);
    int yy2= (int) floor (((double) y2) / PIXEL + 0.5);
    if (is_tuple (t, "penw")) {
      int pos1= xx1 + probe (gl, xx1, yy2,  1, 0);
      int pos2= xx2 + probe (gl, xx2, yy2, -1, 0);
      int w   = max (pos2 - pos1 + 1, 0) * PIXEL;
      return as_string (((double) w) / hunit);
    }
    else {
      int pos1= yy1 + probe (gl, xx1, yy1, 0,  1);
      int pos2= yy2 + probe (gl, xx1, yy2, 0, -1);
      int h   = max (pos2 - pos1 + 1, 0) * PIXEL;
      return as_string (((double) h) / vunit);
    }
  }
  else if (is_tuple (t, "sep-equal", 0)) {
    scheme_tree yt= tuple ("ypos", "=", "0.5", "0.5", "+");
    scheme_tree yb= tuple ("ypos", "=", "0.5", "0.5", "-");
    return exec (tuple ("-", yt, yb));
  }
  else if (is_tuple (t, "frac-width", 0))
    return exec (tuple ("height", tuple ("ver-crop", "minus")));
  else return t;
}

/******************************************************************************
* Check integrity of virtual character
******************************************************************************/

bool
virtual_font_rep::supported (scheme_tree t, bool svg) {
  if (is_atomic (t)) {
    string r= t->label;
    if (r == "#28") r= "(";
    if (r == "#29") r= ")";
    if (N(r)>1) r= "<" * r * ">";
    if (!extend || base_fn->supports (r) || !virt->dict->contains (r))
      return base_fn->supports (r);
    if (!virt->dict->contains (r)) return false;
    return supported (virt->virt_def [virt->dict [r]], svg);
  }

  if (is_func (t, TUPLE, 3) && (is_double (t[0])) && (is_double (t[1])))
    return supported (t[2], svg);

  if (is_tuple (t, "with", 3) && is_atomic (t[1])) {
    tree var= t[1];
    tree val= exec (t[2]);
    return supported (replace (t[3], var, val), svg);
  }

  if (is_tuple (t, "or") && N(t) >= 2) {
    int i, n= N(t);
    for (i=1; i<n; i++)
      if (supported (t[i], svg))
        return true;
    return false;
  }

  if (is_tuple (t, "join") ||
      (is_tuple (t, "intersect", 2) && !svg) ||
      (is_tuple (t, "exclude", 2) && !svg) ||
      (is_tuple (t, "bitmap", 1) && !svg) ||
      is_tuple (t, "glue", 2) ||
      is_tuple (t, "glue*", 2) ||
      is_tuple (t, "glue-above", 2) ||
      is_tuple (t, "glue-below", 2) ||
      is_tuple (t, "row", 2) ||
      is_tuple (t, "stack", 2) ||
      is_tuple (t, "stack-equal", 2) ||
      is_tuple (t, "stack-less", 2) ||
      is_tuple (t, "add", 2) ||
      (is_tuple (t, "bar-right*", 2) && !svg) ||
      (is_tuple (t, "bar-right", 2) && !svg) ||
      (is_tuple (t, "bar-left", 2) && !svg) ||
      (is_tuple (t, "bar-bottom*", 2) && !svg) ||
      (is_tuple (t, "bar-bottom", 2) && !svg) ||
      (is_tuple (t, "bar-top", 2) && !svg)) {
    int i, n= N(t);
    for (i=1; i<n; i++)
      if (!supported (t[i], svg)) return false;
    return true;
  }

  if (is_tuple (t, "glue-above", 3) ||
      is_tuple (t, "glue-below", 3) ||
      is_tuple (t, "stack", 3) ||
      (is_tuple (t, "left-fit", 3) && is_double (t[3])) ||
      (is_tuple (t, "right-fit", 3) && is_double (t[3]))) {
    int i, n= N(t);
    for (i=1; i<n-1; i++)
      if (!supported (t[i], svg)) return false;
    return true;
  }

  if (is_tuple (t, "magnify", 3) ||
      is_tuple (t, "deepen", 3) ||
      is_tuple (t, "widen", 3) ||
      is_tuple (t, "enlarge") ||
      is_tuple (t, "unindent", 1) ||
      is_tuple (t, "unindent*", 1) ||
      is_tuple (t, "crop", 1) ||
      is_tuple (t, "hor-crop", 1) ||
      is_tuple (t, "left-crop", 1) ||
      is_tuple (t, "right-crop", 1) ||
      is_tuple (t, "ver-crop", 1) ||
      is_tuple (t, "top-crop", 1) ||
      is_tuple (t, "bottom-crop", 1) ||
      is_tuple (t, "clip") ||
      is_tuple (t, "part") ||
      is_tuple (t, "copy", 1) ||
      is_tuple (t, "hor-flip", 1) ||
      is_tuple (t, "ver-flip", 1) ||
      is_tuple (t, "rot-left", 1) ||
      is_tuple (t, "rot-right", 1) ||
      (is_tuple (t, "rotate") && N(t) >= 3) ||
      (is_tuple (t, "curly", 1) && !svg) ||
      (is_tuple (t, "bottom-edge") && !svg) ||
      (is_tuple (t, "flood-fill", 3) && !svg) ||
      is_tuple (t, "hor-extend", 3) ||
      is_tuple (t, "hor-extend", 4) ||
      is_tuple (t, "ver-extend", 3) ||
      is_tuple (t, "ver-extend", 4) ||
      is_tuple (t, "ver-take", 3) ||
      is_tuple (t, "ver-take", 4) ||
      (is_tuple (t, "unserif") && !svg) ||
      is_tuple (t, "italic", 3))
    return supported (t[1], svg);

  if ((is_tuple (t, "align") && N(t) >= 3) ||
      (is_tuple (t, "align*") && N(t) >= 3) ||
      is_tuple (t, "scale", 4) ||
      is_tuple (t, "scale*", 4) ||
      (is_tuple (t, "fscale", 5) && !svg) ||
      (is_tuple (t, "fscale*", 5) && !svg) ||
      is_tuple (t, "hor-scale", 2) ||
      is_tuple (t, "pretend", 2) ||
      is_tuple (t, "hor-pretend", 2) ||
      is_tuple (t, "left-pretend", 2) ||
      is_tuple (t, "right-pretend", 2) ||
      is_tuple (t, "ver-pretend", 2) ||
      is_tuple (t, "reslash", 2) ||
      is_tuple (t, "negate", 2) ||
      is_tuple (t, "min-width", 2) ||
      is_tuple (t, "max-width", 2) ||
      is_tuple (t, "min-height", 2) ||
      is_tuple (t, "max-height", 2))
    return supported (t[1], svg) && supported (t[2], svg);

  if (is_tuple (t, "font") && N(t) >= 3) {
    for (int i=2; i<N(t); i++)
      if (is_atomic (t[i]) &&
          occurs ("unicode:" * t[i]->label, base_fn->res_name))
        return supported (t[1], svg);
    return false;
  }

  if (is_tuple (t, "circle", 2) && !svg)
    return true;

  return false;
}

bool
virtual_font_rep::supported (string c, bool svg) {
  hashmap<string,bool>& sup (svg? sup_svg: sup_bit);
  if (!sup->contains (c)) {
    tree t= get_tree (c);
    if (t == "") sup (c)= false;
    else sup (c)= supported (t, svg);
  }
  return sup[c];
}

/******************************************************************************
* Compilation of virtual characters
******************************************************************************/

static void
outer_fit (metric& ex, metric& ey, SI x, SI y) {
  ex->x1= min (ex->x1, x+ ey->x1);
  ex->y1= min (ex->y1, y+ ey->y1);
  ex->x2= max (ex->x2, x+ ey->x2);
  ex->y2= max (ex->y2, y+ ey->y2);
  ex->x3= min (ex->x3, x+ ey->x3);
  ex->y3= min (ex->y3, y+ ey->y3);
  ex->x4= max (ex->x4, x+ ey->x4);
  ex->y4= max (ex->y4, y+ ey->y4);
}

static void
move (metric& ex, SI x, SI y) {
  if (x != 0) {
    ex->x1 += x; ex->x3 += x - PIXEL;
    ex->x2 += x; ex->x4 += x + PIXEL;
  }
  if (y != 0) {
    ex->y1 += y; ex->y3 += y - PIXEL;
    ex->y2 += y; ex->y4 += y + PIXEL;
  }
}

static double
get_magnification (SI w1, SI w2, double sc) {
  if (w1 <= 0 || w2 <= 0 || sc == 0.0) return 1.0;
  double f= ((double) w2) / ((double) w1);
  return exp (sc * log (f));
}

static void
stretch (metric& ex, double mx, double my) {
  if (mx != 1.0) {
    ex->x1=  (SI) floor (mx * ex->x1);
    ex->x2=  (SI) ceil  (mx * ex->x2);
    ex->x3= ((SI) floor (mx * ex->x3)) - PIXEL;
    ex->x4= ((SI) ceil  (mx * ex->x4)) + PIXEL;
  }
  if (my != 1.0) {
    ex->y1=  (SI) floor (my * ex->y1);
    ex->y2=  (SI) ceil  (my * ex->y2);
    ex->y3= ((SI) floor (my * ex->y3)) - PIXEL;
    ex->y4= ((SI) ceil  (my * ex->y4)) + PIXEL;
  }
}

static void
assign (metric& ex, metric ex2) {
  ex->x1= ex2->x1;
  ex->x2= ex2->x2;
  ex->x3= ex2->x3;
  ex->x4= ex2->x4;
  ex->y1= ex2->y1;
  ex->y2= ex2->y2;
  ex->y3= ex2->y3;
  ex->y4= ex2->y4;
}

glyph
virtual_font_rep::compile_bis (scheme_tree t, metric& ex) {
  //cout << "Compile " << t << "\n";

  if (is_atomic (t)) {
    string r= t->label;
    if (r == "#28") r= "(";
    if (r == "#29") r= ")";
    if (N(r)>1) r= "<" * r * ">";
    glyph gl;
    if (!extend || base_fn->supports (r) || !virt->dict->contains (r)) {
      base_fn->get_extents (r, ex);
      gl= base_fn->get_glyph (r);
    }
    else {
      scheme_tree u= virt->virt_def [virt->dict [r]];
      gl= compile (u, ex);
    }
    if (gl->width == 0 && gl->height == 0)
      ex->x1= ex->y1= ex->x2= ex->y2= ex->x3= ex->y3= ex->x4= ex->y4= 0;
    return gl;
  }

  if (is_func (t, TUPLE, 3) &&
      (is_double (t[0])) && (is_double (t[1])))
    {
      SI x= (SI) (as_double (t[0]) * hunit);
      SI y= (SI) (as_double (t[1]) * vunit);
      glyph gl= compile (t[2], ex);
      move (ex, x, y);
      return move (gl, x, y);
    }

  if (is_tuple (t, "with", 3) && is_atomic (t[1])) {
    tree var= t[1];
    tree val= exec (t[2]);
    return compile (replace (t[3], var, val), ex);
  }

  if (is_tuple (t, "or") && N(t) >= 2) {
    int i, n= N(t);
    for (i=1; i<n-1; i++)
      if (supported (t[i], false))
        return compile (t[i], ex);
    return compile (t[n-1], ex);
  }

  if (is_tuple (t, "join")) {
    int i, n= N(t);
    glyph gl1= compile (t[1], ex);
    for (i=2; i<n; i++) {
      metric ey;
      glyph gl2= compile (t[i], ey);
      outer_fit (ex, ey, 0, 0);
      gl1= join (gl1, gl2);
    }
    return gl1;
  }

  if (is_tuple (t, "intersect", 2)) {
    metric ey;
    glyph gl1= compile (t[1], ex);
    glyph gl2= compile (t[2], ey);
    return intersect (gl1, gl2);
  }

  if (is_tuple (t, "exclude", 2)) {
    metric ey;
    glyph gl1= compile (t[1], ex);
    glyph gl2= compile (t[2], ey);
    return exclude (gl1, gl2);
  }

  if (is_tuple (t, "bitmap", 1))
    return compile (t[1], ex);

  if (is_tuple (t, "glue", 2)) {
    metric ey;
    glyph gl1= compile (t[1], ex);
    glyph gl2= compile (t[2], ey);
    SI dx= ex->x2- ((base_fn->wpt*28)>>4);
    outer_fit (ex, ey, dx, 0);
    return join (gl1, move (gl2, dx, 0));
  }

  if (is_tuple (t, "glue*", 2)) {
    metric ey;
    glyph gl1= compile (t[1], ex);
    glyph gl2= compile (t[2], ey);
    SI dx= ex->x2;
    outer_fit (ex, ey, dx, 0);
    return join (gl1, move (gl2, dx, 0));
  }

  if (is_tuple (t, "glue-above", 2) ||
      is_tuple (t, "glue-above", 3)) {
    metric ey;
    glyph gl1= compile (t[1], ex);
    glyph gl2= compile (t[2], ey);
    SI dy= ex->y2 - ey->y1;
    if (N(t) >= 4 && is_double (t[3]))
      dy += (SI) (as_double (t[3]) * vunit);
    outer_fit (ex, ey, 0, dy);
    return join (gl1, move (gl2, 0, dy));
  }

  if (is_tuple (t, "glue-below", 2) ||
      is_tuple (t, "glue-below", 3)) {
    metric ey;
    glyph gl1= compile (t[1], ex);
    glyph gl2= compile (t[2], ey);
    SI dy= ex->y1 - ey->y2;
    if (N(t) >= 4 && is_double (t[3]))
      dy -= (SI) (as_double (t[3]) * vunit);
    outer_fit (ex, ey, 0, dy);
    return join (gl1, move (gl2, 0, dy));
  }

  if (is_tuple (t, "row", 2)) {
    metric ey;
    scheme_tree u= tuple ("glue", tuple ("right-crop", t[1]),
                          tuple ("-0.05", "0", tuple ("left-crop", t[2])));
    glyph gl1= compile (u, ey);
    glyph gl2= compile (tuple ("glue", t[1], t[2]), ex);
    SI delta= (SI) (0.05 * hunit);
    ex->x2 -= delta;
    SI dx= ((ex->x2 - ex->x1) - (ey->x2 - ey->x1)) >> 1;
    return move (gl1, dx, 0);
  }

  if (is_tuple (t, "stack", 2) ||
      is_tuple (t, "stack", 3)) {
    metric ey;
    glyph gl1= compile (t[1], ex);
    glyph gl2= compile (t[2], ey);
    SI dy= ex->y1 - ey->y2;
    SI up= (ey->y2 - ey->y1) >> 1;
    SI lo= 0;
    if (N(t) >= 4 && is_double (t[3]))
      lo= (SI) (as_double (t[3]) * vunit);
    dy -= lo; up += (lo >> 1);
    outer_fit (ex, ey, 0, dy);
    move (ex, 0, up);
    return move (join (gl1, move (gl2, 0, dy)), 0, up);
  }

  if (is_tuple (t, "stack-equal", 2)) {
    scheme_tree d= exec (tuple ("sep-equal"));
    scheme_tree u= tuple ("align", tuple ("stack", t[1], t[2], d),
                          "=", "*", "0.5");
    return compile (u, ex);
  }

  if (is_tuple (t, "stack-less", 2)) {
    scheme_tree d= exec (tuple ("sep-equal"));
    scheme_tree u= tuple ("align", tuple ("stack", t[1], t[2], d),
                          "less", "*", "0.5");
    return compile (u, ex);
  }

  if (is_tuple (t, "right-fit", 3) && is_double (t[3])) {
    metric ey;
    glyph gl1= compile (t[1], ex);
    glyph gl2= compile (t[2], ey);
    SI x2= ex->x2 + ((SI) (as_double (t[3]) * (ey->x2 - ey->x1)));
    SI dx= collision_offset (gl1, gl2, true);
    outer_fit (ex, ey, dx, 0);
    ex->x2= x2;
    return join (gl1, move (gl2, dx, 0));
  }

  if (is_tuple (t, "left-fit", 3) && is_double (t[3]))
    return compile (tuple ("hor-flip",
                           tuple ("right-fit",
                                  tuple ("hor-flip", t[1]),
                                  tuple ("hor-flip", t[2]), t[3])), ex);

  if (is_tuple (t, "bar-right*")) {
    metric ey;
    glyph gl1= compile (t[1], ex);
    glyph gl2= compile (t[2], ey);
    outer_fit (ex, ey, 0, 0);
    return bar_right (gl1, gl2);
  }

  if (is_tuple (t, "bar-left", 2)) {
    tree u1= tuple ("hor-flip", t[1]);
    tree u2= tuple ("align*", tuple ("hor-flip", t[2]), u1, "1", "0.5");
    tree b = tuple ("bar-right*", u1, u2);
    return compile (tuple ("hor-flip", b), ex);
  }

  if (is_tuple (t, "bar-right", 2)) {
    tree u1= t[1];
    tree u2= tuple ("align*", t[2], u1, "1", "0.5");
    tree b = tuple ("bar-right*", u1, u2);
    return compile (b, ex);
  }

  if (is_tuple (t, "bar-bottom*")) {
    metric ey;
    glyph gl1= compile (t[1], ex);
    glyph gl2= compile (t[2], ey);
    outer_fit (ex, ey, 0, 0);
    return bar_bottom (gl1, gl2);
  }

  if (is_tuple (t, "bar-bottom", 2)) {
    tree u1= t[1];
    tree u2= tuple ("align*", t[2], u1, "0.5", "0");
    tree b = tuple ("bar-bottom*", u1, u2);
    return compile (b, ex);
  }

  if (is_tuple (t, "bar-top", 2)) {
    tree u1= tuple ("ver-flip", t[1]);
    tree u2= tuple ("align*", tuple ("ver-flip", t[2]), u1, "0.5", "0");
    tree b = tuple ("bar-bottom*", u1, u2);
    return compile (tuple ("ver-flip", b), ex);
  }

  if (is_tuple (t, "add", 2)) {
    metric ey;
    glyph gl1= compile (t[1], ex);
    glyph gl2= compile (t[2], ey);
    SI dx= ((ex->x1+ ex->x2- ey->x1- ey->x2) >> 1);
    outer_fit (ex, ey, dx, 0);
    return join (gl1, move (gl2, dx, 0));
  }

  if (is_tuple (t, "magnify", 3)) {
    glyph gl= compile (t[1], ex);
    double mx= 1.0, my= 1.0;
    if (is_double (t[2])) mx= as_double (t[2]);
    if (is_double (t[3])) my= as_double (t[3]);
    stretch (ex, mx, my);
    return stretched (gl, mx, my);
  }

  if (is_tuple (t, "deepen", 3)) {
    glyph gl= compile (t[1], ex);
    double my= 1.0;
    SI pw= 5*PIXEL;
    if (is_double (t[2])) my= as_double (t[2]);
    if (is_double (t[3])) pw= (SI) floor (as_double (t[3]) * vunit);
    stretch (ex, 1.0, my);
    return deepen (gl, my, pw);
  }

  if (is_tuple (t, "widen", 3)) {
    glyph gl= compile (t[1], ex);
    double mx= 1.0;
    SI pw= 5*PIXEL;
    if (is_double (t[2])) mx= as_double (t[2]);
    if (is_double (t[3])) pw= (SI) floor (as_double (t[3]) * hunit);
    stretch (ex, mx, 1.0);
    return widen (gl, mx, pw);
  }

  if (is_tuple (t, "enlarge")) {
    glyph gl= compile (t[1], ex);
    if (N(t)>2) ex->x1 -= (SI) (as_double (t[2]) * hunit);
    if (N(t)>3) ex->x2 += (SI) (as_double (t[3]) * hunit);
    if (N(t)>4) ex->y1 -= (SI) (as_double (t[4]) * vunit);
    if (N(t)>5) ex->y2 += (SI) (as_double (t[5]) * vunit);
    return gl;
  }

  if (is_tuple (t, "unindent", 1)) {
    glyph gl= compile (t[1], ex);
    SI dx= -ex->x1;
    move (ex, dx, 0);
    return move (gl, dx, 0);
  }

  if (is_tuple (t, "unindent*", 1)) {
    glyph gl= compile (t[1], ex);
    SI dx= -ex->x2;
    move (ex, dx, 0);
    return move (gl, dx, 0);
  }

  if (is_tuple (t, "crop", 1) ||
      is_tuple (t, "hor-crop", 1) ||
      is_tuple (t, "left-crop", 1) ||
      is_tuple (t, "right-crop", 1) ||
      is_tuple (t, "ver-crop", 1) ||
      is_tuple (t, "top-crop", 1) ||
      is_tuple (t, "bottom-crop", 1)) {
    glyph gl= simplify (compile (t[1], ex));
    SI x1, y1, x2, y2;
    get_bounding_box (gl, x1, y1, x2, y2);
    if (is_tuple (t, "crop") ||
        is_tuple (t, "hor-crop") ||
        is_tuple (t, "left-crop"))
      ex->x1= ex->x3= x1;
    if (is_tuple (t, "crop") ||
        is_tuple (t, "hor-crop") ||
        is_tuple (t, "right-crop"))
      ex->x2= ex->x4= x2;
    if (is_tuple (t, "crop") ||
        is_tuple (t, "ver-crop") ||
        is_tuple (t, "bottom-crop"))
      ex->y1= ex->y3= y1;
    if (is_tuple (t, "crop") ||
        is_tuple (t, "ver-crop") ||
        is_tuple (t, "top-crop"))
      ex->y2= ex->y4= y2;
    return gl;
  }

  if (is_tuple (t, "clip")) {
    glyph gl= compile (t[1], ex);
    SI x1, y1, x2, y2;
    get_bounding_box (gl, x1, y1, x2, y2);
    if (N(t)>2 && t[2]!="*")
      x1= ex->x1= ex->x3= (SI) (as_double (t[2]) * hunit);
    if (N(t)>3 && t[3]!="*")
      x2= ex->x2= ex->x4= (SI) (as_double (t[3]) * hunit);
    if (N(t)>4 && t[4]!="*")
      y1= ex->y1= ex->y3= (SI) (as_double (t[4]) * vunit);
    if (N(t)>5 && t[5]!="*")
      y2= ex->y2= ex->y4= (SI) (as_double (t[5]) * vunit);
    return clip (gl, x1, y1, x2, y2);
  }

  if (is_tuple (t, "part")) {
    glyph gl= compile (t[1], ex);
    SI ox= ex->x1, gw= ex->x2 - ex->x1;
    SI oy= ex->y1, gh= ex->y2 - ex->y1;
    SI x1, y1, x2, y2;
    get_bounding_box (gl, x1, y1, x2, y2);
    if (N(t)>2 && t[2]!="*")
      x1= ex->x1= ex->x3= ox + (SI) (as_double (t[2]) * gw);
    if (N(t)>3 && t[3]!="*")
      x2= ex->x2= ex->x4= ox + (SI) (as_double (t[3]) * gw);
    if (N(t)>4 && t[4]!="*")
      y1= ex->y1= ex->y3= oy + (SI) (as_double (t[4]) * gh);
    if (N(t)>5 && t[5]!="*")
      y2= ex->y2= ex->y4= oy + (SI) (as_double (t[5]) * gh);
    glyph cgl= clip (gl, x1, y1, x2, y2);
    SI dx= 0, dy= 0;
    if (N(t)>6) dx= (SI) (as_double (t[6]) * gw);
    if (N(t)>7) dy= (SI) (as_double (t[7]) * gh);
    if (dx == 0 && dy == 0) return cgl;
    if (dx != 0) {
      ex->x1 += dx; ex->x3 += dx - PIXEL;
      ex->x2 += dx; ex->x4 += dx + PIXEL;
    }
    if (dy != 0) {
      ex->y1 += dy; ex->y3 += dy - PIXEL;
      ex->y2 += dy; ex->y4 += dy + PIXEL;
    }
    return move (cgl, dx, dy);
  }

  if (is_tuple (t, "copy", 1))
    return copy (compile (t[1], ex));

  if (is_tuple (t, "hor-flip", 1))
    return hor_flip (compile (t[1], ex));

  if (is_tuple (t, "ver-flip", 1))
    return ver_flip (compile (t[1], ex));

  if (is_tuple (t, "rot-left", 1)) {
    metric ey;
    glyph gl= pos_rotate (compile (t[1], ey));
    ex->x1= 0;
    ex->y1= 0;
    ex->x2= ey->y2- ey->y1;
    ex->y2= ey->x2- ey->x1;
    ex->x3= ey->y2- ey->y4;
    ex->y3= ey->x3- ey->x1;
    ex->x4= ey->y2- ey->y3;
    ex->y4= ey->x4- ey->x1;
    return move (gl, ey->y2, -ey->x1);
  }

  if (is_tuple (t, "rot-right", 1)) {
    metric ey;
    glyph gl= pos_rotate (pos_rotate (pos_rotate (compile (t[1], ey))));
    ex->x1= 0;
    ex->y1= 0;
    ex->x2= ey->y2- ey->y1;
    ex->y2= ey->x2- ex->x1;
    ex->x3= ey->y3- ey->y1;
    ex->y3= ey->x2- ey->x4;
    ex->x4= ey->y4- ey->y1;
    ex->y4= ey->x2- ey->x3;
    return move (gl, -ey->y1, ey->x2);
  }

  if (is_tuple (t, "rotate") && N(t) >= 3) {
    metric ey;
    glyph gl= compile (t[1], ey);
    double angle= 0.0;
    if (N(t) >= 3 && is_double (t[2])) angle= as_double (t[2]);
    double xf= 0.5, yf= 0.5;
    if (N(t) >= 4 && is_double (t[3])) xf= as_double (t[3]);
    if (N(t) >= 5 && is_double (t[4])) yf= as_double (t[4]);
    SI ox= ey->x1 + ((SI) (xf * (ey->x2 - ey->x1)));
    SI oy= ey->y1 + ((SI) (yf * (ey->y2 - ey->y1)));
    rotate (ex, ey, angle, ox, oy);
    return rotate (gl, angle, ox, oy);
  }

  if (is_tuple (t, "curly", 1)) {
    glyph gl= compile (t[1], ex);
    return curly (gl);
  }

  if (is_tuple (t, "bottom-edge") && N(t) >= 2) {
    glyph gl= compile (t[1], ex);
    SI penh = 5 * PIXEL;
    SI keepy= ((ex->y3 + ex->y4) >> 1) - (ex->y4 - ex->y3) / 20;
    if (N(t) >= 3 && is_double (t[2]))
      penh = (SI) floor (as_double (t[2]) * vunit);
    if (N(t) >= 4 && is_double (t[3]))
      keepy= (SI) floor (as_double (t[3]) * vunit);
    return bottom_edge (gl, penh, keepy);
  }

  if (is_tuple (t, "flood-fill", 3)) {
    glyph gl= compile (t[1], ex);
    SI x= (ex->x3 + ex->x4) >> 1;
    SI y= (ex->y3 + ex->y4) >> 1;
    if (is_double (t[2]))
      x= (SI) floor (ex->x3 + as_double (t[2]) * (ex->x4 - ex->x3));
    if (is_double (t[3]))
      y= (SI) floor (ex->y3 + as_double (t[3]) * (ex->y4 - ex->y3));
    return flood_fill (gl, x, y);
  }

  if (is_tuple (t, "hor-extend", 3) || is_tuple (t, "hor-extend", 4)) {
    glyph gl= compile (t[1], ex);
    int pos= (int) (as_double (t[2]) * gl->width);
    SI  add= (SI)  (as_double (t[3]) * hunit);
    if (is_tuple (t, "hor-extend", 4))
      add= (SI)  (as_double (t[3]) * as_double (t[4]) * hunit);
    int by = add / PIXEL;
    if (pos < 0) pos= 0;
    if (pos >= gl->width) pos= gl->width-1;
    ex->x2 += add;
    ex->x4 += by * PIXEL;
    return hor_extend (gl, pos, by);
  }

  if (is_tuple (t, "ver-extend", 3) || is_tuple (t, "ver-extend", 4)) {
    glyph gl= compile (t[1], ex);
    int pos= (int) ((1.0 - as_double (t[2])) * gl->height);
    SI  add= (SI)  (as_double (t[3]) * vunit);
    if (is_tuple (t, "ver-extend", 4))
      add= (SI)  (as_double (t[3]) * as_double (t[4]) * vunit);
    int by = add / PIXEL;
    if (pos < 0) pos= 0;
    if (pos >= gl->height) pos= gl->height-1;
    ex->y1 -= add;
    ex->y3 -= by * PIXEL;
    return ver_extend (gl, pos, by);
  }

  if (is_tuple (t, "ver-take", 3) || is_tuple (t, "ver-take", 4)) {
    glyph gl= compile (t[1], ex);
    int pos= (int) ((1.0 - as_double (t[2])) * gl->height);
    SI  add= (SI)  (as_double (t[3]) * (ex->y2 - ex->y1));
    if (is_tuple (t, "ver-take", 4))
      add= (SI) (as_double (t[3]) * as_double (t[4]) * (ex->y2 - ex->y1));
    int nr = add / PIXEL;
    if (pos < 0) pos= 0;
    if (pos >= gl->height) pos= gl->height-1;
    ex->y1= -add;
    ex->y2= 0;
    ex->y3= -nr * PIXEL;
    ex->y4= 0;
    return ver_take (gl, pos, nr);
  }

  if (is_tuple (t, "unserif") && N(t) >= 2) {
    glyph gl= compile (t[1], ex);
    string s;
    if (N(t) >= 3 && is_atomic (t[2])) s= t[2]->label;
    else if (is_atomic (t[1])) s= t[1]->label;
    if (N(s) != 1) s= "<" * s * ">";
    int code= get_utf8_code (s);
    SI penw= (SI) floor (as_double (exec (tuple ("frac-width"))) * vunit);
    glyph uns= unserif (gl, code, penw);
    normalize_borders (uns, ex);
    return uns;
  }

  if (is_tuple (t, "align") && N(t) >= 3) { // logical alignment
    metric ex2;
    glyph gl= compile (t[1], ex);
    glyph gl2= compile (t[2], ex2);
    double xa= 0.0, xa2= 0.0, ya= 0.0, ya2= 0.0;
    if (N(t) >= 4 && is_double (t[3])) xa= xa2= as_double (t[3]);
    if (N(t) >= 5 && is_double (t[4])) ya= ya2= as_double (t[4]);
    if (N(t) >= 6 && is_double (t[5])) xa2= as_double (t[5]);
    if (N(t) >= 7 && is_double (t[6])) ya2= as_double (t[6]);
    SI ax = (SI) (ex ->x1 + xa  * (ex ->x2 - ex ->x1));
    SI ax2= (SI) (ex2->x1 + xa2 * (ex2->x2 - ex2->x1));
    SI ay = (SI) (ex ->y1 + ya  * (ex ->y2 - ex ->y1));
    SI ay2= (SI) (ex2->y1 + ya2 * (ex2->y2 - ex2->y1));
    SI dx = ax2 - ax;
    SI dy = ay2 - ay;
    if (N(t) >= 4 && t[3] == "*") dx= 0;
    if (N(t) >= 5 && t[4] == "*") dy= 0;
    move (ex, dx, dy);
    return move (gl, dx, dy);
  }

  if (is_tuple (t, "align*") && N(t) >= 3) { // ink alignment
    metric ex1, ex2;
    glyph gl = compile (t[1], ex);
    glyph gl1= compile (tuple ("crop", t[1]), ex1);
    glyph gl2= compile (tuple ("crop", t[2]), ex2);
    double xa1= 0.0, xa2= 0.0, ya1= 0.0, ya2= 0.0;
    if (N(t) >= 4 && is_double (t[3])) xa1= xa2= as_double (t[3]);
    if (N(t) >= 5 && is_double (t[4])) ya1= ya2= as_double (t[4]);
    if (N(t) >= 6 && is_double (t[5])) xa2= as_double (t[5]);
    if (N(t) >= 7 && is_double (t[6])) ya2= as_double (t[6]);
    SI ax1= (SI) (ex1->x3 + xa1 * (ex1->x4 - ex1->x3));
    SI ax2= (SI) (ex2->x3 + xa2 * (ex2->x4 - ex2->x3));
    SI ay1= (SI) (ex1->y3 + ya1 * (ex1->y4 - ex1->y3));
    SI ay2= (SI) (ex2->y3 + ya2 * (ex2->y4 - ex2->y3));
    SI dx = ax2 - ax1;
    SI dy = ay2 - ay1;
    if (N(t) >= 4 && t[3] == "*") dx= 0;
    if (N(t) >= 5 && t[4] == "*") dy= 0;
    move (ex, dx, dy);
    return move (gl, dx, dy);
  }

  if (is_tuple (t, "scale", 4) || is_tuple (t, "scale*", 4) ||
      is_tuple (t, "fscale", 5) || is_tuple (t, "fscale*", 5)) {
    metric ex2;
    glyph gl = compile (t[1], ex);
    glyph gl2= compile (t[2], ex2);
    double sx= 0.0, sy= 0.0;
    if (N(t) >= 4 && t[3] != "*" && is_double (t[3])) sx= as_double (t[3]);
    if (N(t) >= 5 && t[4] != "*" && is_double (t[4])) sy= as_double (t[4]);
    SI w = ex ->x2 - ex ->x1;
    SI w2= ex2->x2 - ex2->x1;
    SI h = ex ->y2 - ex ->y1;
    SI h2= ex2->y2 - ex2->y1;
    if (is_tuple (t, "scale*") || is_tuple (t, "fscale*")) {
      w = ex ->x4 - ex ->x3;
      w2= ex2->x4 - ex2->x3;
      h = ex ->y4 - ex ->y3;
      h2= ex2->y4 - ex2->y3;
    }
    double mx= get_magnification (w, w2, sx);
    double my= get_magnification (h, h2, sy);
    if (N(t) >= 4 && t[3] == "@") sx= sy;
    if (N(t) >= 5 && t[4] == "@") sy= sx;
    stretch (ex, mx, my);
    if (is_tuple (t, "scale", 4) || is_tuple (t, "scale*", 4))
      return stretched (gl, mx, my);
    else {
      // Faithful width preserving scaling
      SI penw= (SI) floor (hunit * as_double (t[5]));
      SI penh= (SI) floor (vunit * as_double (t[5]));
      glyph r= gl;
      if (mx != 1.0) r= widen  (gl, mx, penw);
      if (my != 1.0) r= deepen (gl, my, penh);
      return r;
    }
  }

  if (is_tuple (t, "hor-scale", 2)) {
    scheme_tree ct1= tuple ("hor-crop", t[1]);
    scheme_tree ct2= tuple ("hor-crop", t[2]);
    scheme_tree u  = tuple ("scale", ct1, ct2, "1", "*");
    scheme_tree v  = tuple ("align", u, t[2], "0.5", "0.5");
    return compile (v, ex);
  }

  if (is_tuple (t, "pretend", 2) ||
      is_tuple (t, "hor-pretend", 2) ||
      is_tuple (t, "left-pretend", 2) ||
      is_tuple (t, "right-pretend", 2) ||
      is_tuple (t, "ver-pretend", 2)) {
    metric ex2;
    glyph gl = compile (t[1], ex);
    glyph gl2= compile (t[2], ex2);
    if (is_tuple (t, "pretend") || is_tuple (t, "hor-pretend")) {
      ex->x1= ex2->x1;
      ex->x2= ex2->x2;
    }
    else if (is_tuple (t, "left-pretend")) ex->x1= ex2->x1;
    else if (is_tuple (t, "right-pretend")) ex->x2= ex2->x2;
    if (is_tuple (t, "pretend") || is_tuple (t, "ver-pretend")) {
      ex->y1= ex2->y1;
      ex->y2= ex2->y2;
    }
    return gl;
  }

  if (is_tuple (t, "reslash", 2)) {
    metric ex1, ex2;
    glyph gl1= compile (t[1], ex1);
    glyph gl2= compile (t[2], ex2);
    frame fr = reslash (ex1, ex2);
    transform (ex, ex1, fr);
    point p1= fr (point ((double) ex1->x1, (double) ex1->y1));
    point p2= point ((double) ex2->x1, (double) ex2->y1);
    point dp= p2 - p1;
    SI dx= floor (dp[0]);
    SI dy= floor (dp[1]);
    move (ex, dx, dy);
    ex->x1= ex2->x1;
    ex->y1= ex2->y1;
    ex->x2= ex2->x2;
    ex->y2= ex2->y2;
    return move (transform (gl1, fr), dx, dy);
  }

  if (is_tuple (t, "negate", 2)) {
    tree bar= t[2];
    metric ex1, ex2;
    get_metric (t[1], ex1);
    get_metric (t[2], ex2);
    SI h1 = ex1->y4 - ex1->y3;
    SI h2 = ex2->y4 - ex2->y3;
    SI sep= (SI) floor (as_double (exec (tuple ("sep-equal"))) * vunit);
    if (h2 < h1+sep || h2 >= h1+4*sep) {
      double mag= ((double) (h1+2*sep)) / ((double) h2);
      bar= tuple ("magnify", bar, "1", as_string (mag));
    }
    scheme_tree a= tuple ("align", bar, t[1], "0.5", "0.5");
    scheme_tree u= tuple ("join", t[1], a);
    scheme_tree p= tuple ("pretend", u, t[1]);
    return compile (p, ex);
  }

  if (is_tuple (t, "min-width", 2)) {
    metric ex2;
    glyph gl = compile (t[1], ex);
    glyph gl2= compile (t[2], ex2);
    if ((ex->x2 - ex->x1) <= (ex2->x2 - ex2->x1)) return gl;
    else { assign (ex, ex2); return gl2; }
  }

  if (is_tuple (t, "max-width", 2)) {
    metric ex2;
    glyph gl = compile (t[1], ex);
    glyph gl2= compile (t[2], ex2);
    if ((ex->x2 - ex->x1) >= (ex2->x2 - ex2->x1)) return gl;
    else { assign (ex, ex2); return gl2; }
  }

  if (is_tuple (t, "min-height", 2)) {
    metric ex2;
    glyph gl = compile (t[1], ex);
    glyph gl2= compile (t[2], ex2);
    if ((ex->y2 - ex->y1) <= (ex2->y2 - ex2->y1)) return gl;
    else { assign (ex, ex2); return gl2; }
  }

  if (is_tuple (t, "max-height", 2)) {
    metric ex2;
    glyph gl = compile (t[1], ex);
    glyph gl2= compile (t[2], ex2);
    if ((ex->y2 - ex->y1) >= (ex2->y2 - ex2->y1)) return gl;
    else { assign (ex, ex2); return gl2; }
  }

  if (is_tuple (t, "font") && N(t) >= 3)
    return compile (t[1], ex);

  if (is_tuple (t, "italic", 3))
    return compile (t[1], ex);

  if (is_tuple (t, "circle", 2)) {
    SI r= (SI) floor (as_double (t[1]) * hunit);
    SI w= (SI) floor (as_double (t[2]) * hunit);
    SI R= r + (w >> 1);
    ex->x1= ex->x3= ex->y1= ex->y3= -R;
    ex->x2= ex->x4= ex->y2= ex->y4=  R;
    return circle_glyph (r, w);
  }

  failed_error << "TeXmacs] The defining tree is " << t << "\n";
  FAILED ("invalid virtual character");
  return glyph ();
}

glyph
virtual_font_rep::compile (scheme_tree t, metric& ex) {
  if (trg->contains (t)) {
    ex[0]= trm[t];
    return trg[t];
  }
  glyph r= compile_bis (t, ex);
  trm(t)= ex[0];
  if (is_tuple (t, "curly"))
    trg(t)= r;
  return r;
}

void
virtual_font_rep::get_metric (scheme_tree t, metric& ex) {
  if (trm->contains (t)) ex[0]= trm[t];
  else (void) compile (t, ex);
}

/******************************************************************************
* Direct drawing of virtual fonts using vector graphics
******************************************************************************/

void
virtual_font_rep::draw_tree (renderer ren, scheme_tree t, SI x, SI y) {
  if (is_atomic (t)) {
    string r= t->label;
    if (r == "#28") r= "(";
    if (r == "#29") r= ")";
    if (N(r)>1) r= "<" * r * ">";
    if (!extend || base_fn->supports (r) || !virt->dict->contains (r))
      base_fn->draw (ren, r, x, y);
    else {
      scheme_tree u= virt->virt_def [virt->dict [r]];
      draw_tree (ren, u, x, y);
    }
    return;
  }
  
  if (is_func (t, TUPLE, 3) &&
      (is_double (t[0])) && (is_double (t[1])))
    {
      SI dx= (SI) (as_double (t[0]) * hunit);
      SI dy= (SI) (as_double (t[1]) * vunit);
      draw_tree (ren, t[2], x+dx, y+dy);
      return;
    }

  if (is_tuple (t, "with", 3) && is_atomic (t[1])) {
    tree var= t[1];
    tree val= exec (t[2]);
    draw_tree (ren, replace (t[3], var, val), x, y);
    return;
  }

  if (is_tuple (t, "or") && N(t) >= 2) {
    int i, n= N(t);
    for (i=1; i<n-1; i++)
      if (supported (t[i], false)) {
        draw_tree (ren, t[i], x, y);
        return;
      }
    draw_tree (ren, t[n-1], x, y);
    return;
  }

  if (is_tuple (t, "join")) {
    int i, n= N(t);
    for (i=1; i<n; i++)
      draw_tree (ren, t[i], x, y);
    return;
  }

  if (is_tuple (t, "bitmap", 1)) {
    draw_tree (ren, t[1], x, y);
    return;
  }

  if (is_tuple (t, "glue", 2)) {
    metric ex;
    get_metric (t[1], ex);
    SI dx= ex->x2- ((base_fn->wpt*28)>>4);
    draw_tree (ren, t[1], x, y);
    draw_tree (ren, t[2], x + dx, y);
    return;
  }

  if (is_tuple (t, "glue*", 2)) {
    metric ex;
    get_metric (t[1], ex);
    SI dx= ex->x2;
    draw_tree (ren, t[1], x, y);
    draw_tree (ren, t[2], x + dx, y);
    return;
  }

  if (is_tuple (t, "glue-above", 2) ||
      is_tuple (t, "glue-above", 3)) {
    metric ex, ey;
    get_metric (t[1], ex);
    get_metric (t[2], ey);
    SI dy= ex->y2 - ey->y1;
    if (N(t) >= 4 && is_double (t[3]))
      dy += (SI) (as_double (t[3]) * vunit);
    draw_tree (ren, t[1], x, y);
    draw_tree (ren, t[2], x, y + dy);
    return;
  }

  if (is_tuple (t, "glue-below", 2) ||
      is_tuple (t, "glue-below", 3)) {
    metric ex, ey;
    get_metric (t[1], ex);
    get_metric (t[2], ey);
    SI dy= ex->y1 - ey->y2;
    if (N(t) >= 4 && is_double (t[3]))
      dy -= (SI) (as_double (t[3]) * vunit);
    draw_tree (ren, t[1], x, y);
    draw_tree (ren, t[2], x, y + dy);
    return;
  }

  if (is_tuple (t, "row", 2)) {
    metric ex, ey;
    scheme_tree u= tuple ("glue", tuple ("right-crop", t[1]),
                          tuple ("-0.05", "0", tuple ("left-crop", t[2])));
    get_metric (u, ey);
    get_metric (tuple ("glue", t[1], t[2]), ex);
    SI delta= (SI) (0.05 * hunit);
    ex->x2 -= delta;
    SI dx= ((ex->x2 - ex->x1) - (ey->x2 - ey->x1)) >> 1;
    draw_tree (ren, u, x + dx, y);
    return;
  }

  if (is_tuple (t, "stack", 2) ||
      is_tuple (t, "stack", 3)) {
    metric ex, ey;
    get_metric (t[1], ex);
    get_metric (t[2], ey);
    SI dy= ex->y1 - ey->y2;
    SI up= (ey->y2 - ey->y1) >> 1;
    SI lo= 0;
    if (N(t) >= 4 && is_double (t[3]))
      lo= (SI) (as_double (t[3]) * vunit);
    dy -= lo; up += (lo >> 1);
    draw_tree (ren, t[1], x, y + up);
    draw_tree (ren, t[2], x, y + dy + up);
    return;
  }

  if (is_tuple (t, "stack-equal", 2)) {
    scheme_tree d= exec (tuple ("sep-equal"));
    scheme_tree u= tuple ("align", tuple ("stack", t[1], t[2], d),
                          "=", "*", "0.5");
    draw_tree (ren, u, x, y);
    return;
  }

  if (is_tuple (t, "stack-less", 2)) {
    scheme_tree d= exec (tuple ("sep-equal"));
    scheme_tree u= tuple ("align", tuple ("stack", t[1], t[2], d),
                          "less", "*", "0.5");
    draw_tree (ren, u, x, y);
    return;
  }

  if (is_tuple (t, "right-fit", 3) && is_double (t[3])) {
    metric ex, ey;
    glyph gl1= compile (t[1], ex);
    glyph gl2= compile (t[2], ey);
    SI dx= collision_offset (gl1, gl2, true);
    dx += 2*PIXEL; // FIXME: needed to counter rounding errors
    draw_tree (ren, t[1], x, y);
    draw_tree (ren, t[2], x + dx, y);
    return;
  }

  if (is_tuple (t, "left-fit", 3) && is_double (t[3])) {
    draw_tree (ren, tuple ("hor-flip",
                           tuple ("right-fit",
                                  tuple ("hor-flip", t[1]),
                                  tuple ("hor-flip", t[2]), t[3])), x, y);
    return;
  }

  if (is_tuple (t, "add", 2)) {
    metric ex, ey;
    get_metric (t[1], ex);
    get_metric (t[2], ey);
    SI dx= ((ex->x1+ ex->x2- ey->x1- ey->x2) >> 1);
    draw_tree (ren, t[1], x, y);
    draw_tree (ren, t[2], x + dx, y);
    return;
  }

  if (is_tuple (t, "magnify", 3)) {
    metric ex;
    get_metric (t[1], ex);
    double mx= 1.0, my= 1.0;
    if (is_double (t[2])) mx= as_double (t[2]);
    if (is_double (t[3])) my= as_double (t[3]);
    ren->move_origin (x, y);
    ren->set_transformation (scaling (point (mx, my), point (0.0, 0.0)));
    draw_tree (ren, t[1], 0, 0);
    ren->reset_transformation ();
    ren->move_origin (-x, -y);
    return;
  }

  if (is_tuple (t, "enlarge")) {
    draw_tree (ren, t[1], x, y);
    return;
  }

  if (is_tuple (t, "unindent", 1)) {
    metric ex;
    get_metric (t[1], ex);
    draw_tree (ren, t[1], x - ex->x1, y);
    return;
  }

  if (is_tuple (t, "unindent*", 1)) {
    metric ex;
    get_metric (t[1], ex);
    draw_tree (ren, t[1], x - ex->x2, y);
    return;
  }

  if (is_tuple (t, "crop", 1) ||
      is_tuple (t, "hor-crop", 1) ||
      is_tuple (t, "left-crop", 1) ||
      is_tuple (t, "right-crop", 1) ||
      is_tuple (t, "ver-crop", 1) ||
      is_tuple (t, "top-crop", 1) ||
      is_tuple (t, "bottom-crop", 1)) {
    draw_tree (ren, t[1], x, y);
    return;
  }

  if (is_tuple (t, "clip")) {
    metric ex;
    get_metric (t[1], ex);
    if (N(t)>2 && t[2]!="*") ex->x3= (SI) (as_double (t[2]) * hunit);
    if (N(t)>3 && t[3]!="*") ex->x4= (SI) (as_double (t[3]) * hunit);
    if (N(t)>4 && t[4]!="*") ex->y3= (SI) (as_double (t[4]) * vunit);
    if (N(t)>5 && t[5]!="*") ex->y4= (SI) (as_double (t[5]) * vunit);
    draw_clipped (ren, t[1], x, y, ex->x3, ex->y3, ex->x4, ex->y4);
    return;
  }

  if (is_tuple (t, "part")) {
    metric ex;
    get_metric (t[1], ex);
    SI ox= ex->x1, gw= ex->x2 - ex->x1;
    SI oy= ex->y1, gh= ex->y2 - ex->y1;
    if (N(t)>2 && t[2]!="*") ex->x1= ex->x3= ox + (SI) (as_double (t[2]) * gw);
    if (N(t)>3 && t[3]!="*") ex->x2= ex->x4= ox + (SI) (as_double (t[3]) * gw);
    if (N(t)>4 && t[4]!="*") ex->y1= ex->y3= oy + (SI) (as_double (t[4]) * gh);
    if (N(t)>5 && t[5]!="*") ex->y2= ex->y4= oy + (SI) (as_double (t[5]) * gh);
    SI dx= 0, dy= 0;
    if (N(t)>6) dx= (SI) (as_double (t[6]) * gw);
    if (N(t)>7) dy= (SI) (as_double (t[7]) * gh);
    draw_clipped (ren, t[1], x + dx, y + dy, ex->x3, ex->y3, ex->x4, ex->y4);
    return;
  }

  if (is_tuple (t, "copy", 1))
    draw_tree (ren, t[1], x, y);

  if (is_tuple (t, "hor-flip", 1)) {
    metric ex;
    get_metric (t[1], ex);
    SI ox= x + ex->x3 + ex->x4;
    frame f= scaling (point (-1.0, 1.0), point ((double) ox, 0.0));
    draw_transformed (ren, t[1], 0, y, f);
    return;
  }

  if (is_tuple (t, "ver-flip", 1)) {
    metric ex;
    get_metric (t[1], ex);
    SI oy= y + ex->y3 + ex->y4;
    frame f= scaling (point (1.0, -1.0), point (0.0, (double) oy));
    draw_transformed (ren, t[1], x, 0, f);
    return;
  }

  if (is_tuple (t, "rot-left", 1)) {
    // FIXME: check that we should not use physical metrics
    // as in the case of hor-flip and ver-flip
    metric ex;
    get_metric (t[1], ex);
    //cout << "left " << (x/PIXEL) << ", " << (y/PIXEL) << "; "
    //     << (ex->x1/PIXEL) << ", " << (ex->y1/PIXEL) << "; "
    //     << (ex->x2/PIXEL) << ", " << (ex->y2/PIXEL) << "\n";
    SI ox= x + ex->x1;
    SI oy= y + ex->y2;
    frame f= rotation_2D (point ((double) ox, (double) oy), 1.57079632679);
    draw_transformed (ren, t[1], x - ex->y2, y + ex->x1, f);
    return;
  }

  if (is_tuple (t, "rot-right", 1)) {
    // FIXME: check that we should not use physical metrics
    // as in the case of hor-flip and ver-flip
    metric ex;
    get_metric (t[1], ex);
    //cout << "right " << (x/PIXEL) << ", " << (y/PIXEL) << "; "
    //     << (ex->x1/PIXEL) << ", " << (ex->y1/PIXEL) << "; "
    //     << (ex->x2/PIXEL) << ", " << (ex->y2/PIXEL) << "\n";
    SI ox= x + ex->x2;
    SI oy= y + ex->y1;
    frame f= rotation_2D (point ((double) ox, (double) oy), -1.57079632679);
    draw_transformed (ren, t[1], x + ex->y1, y - ex->x2, f);
    return;
  }

  if (is_tuple (t, "rotate") && N(t) >= 3) {
    metric ey;
    get_metric (t[1], ey);
    double angle= 0.0;
    if (N(t) >= 3 && is_double (t[2])) angle= as_double (t[2]);
    double xf= 0.5, yf= 0.5;
    if (N(t) >= 4 && is_double (t[3])) xf= as_double (t[3]);
    if (N(t) >= 5 && is_double (t[4])) yf= as_double (t[4]);
    SI ox= x + ey->x1 + ((SI) (xf * (ey->x2 - ey->x1)));
    SI oy= y + ey->y1 + ((SI) (yf * (ey->y2 - ey->y1)));
    frame fr= rotation_2D (point ((double) ox, (double) oy), angle);
    draw_transformed (ren, t[1], x, y, fr);
    return;
  }

  if (is_tuple (t, "hor-extend", 3) || is_tuple (t, "hor-extend", 4)) {
    metric ex;
    get_metric (t[1], ex);
    SI pos= (SI) (as_double (t[2]) * (ex->x2 - ex->x1));
    SI add= (SI) (as_double (t[3]) * hunit);
    if (is_tuple (t, "hor-extend", 4))
      add= (SI) (as_double (t[3]) * as_double (t[4]) * hunit);
    if (add > 0 && ex->x2 > ex->x1) {
      SI  w = ex->x2 - ex->x1;
      int n = (int) ((20 * add + w - 1) / w);
      SI  dx= (add + n - 1) / n;
      SI  hx= (add + 2*n - 1) / (2*n);
      for (int i=0; i<n; i++)
        draw_clipped (ren, t[1], x + hx + i*dx, y,
                      ex->x3 + pos - hx, ex->y3, ex->x3 + pos + hx, ex->y4);
    }
    draw_clipped (ren, t[1], x, y, ex->x3, ex->y3, ex->x3 + pos, ex->y4);
    draw_clipped (ren, t[1], x + add, y, ex->x3 + pos, ex->y3, ex->x4, ex->y4);
    return;
  }

  if (is_tuple (t, "ver-extend", 3) || is_tuple (t, "ver-extend", 4)) {
    metric ex;
    get_metric (t[1], ex);
    SI pos= (SI) ((1.0 - as_double (t[2])) * (ex->y2 - ex->y1));
    SI add= (SI) (as_double (t[3]) * vunit);
    if (is_tuple (t, "ver-extend", 4))
      add= (SI) (as_double (t[3]) * as_double (t[4]) * vunit);
    if (add > 0 && ex->y2 > ex->y1) {
      SI  h = ex->y2 - ex->y1;
      int n = (int) ((20 * add + h - 1) / h);
      SI  dy= (add + n - 1) / n;
      SI  hy= (add + 2*n - 1) / (2*n);
      for (int i=0; i<n; i++)
        draw_clipped (ren, t[1], x, y + hy + i*dy - add,
                      ex->x3, ex->y3 + pos - hy, ex->x4, ex->y3 + pos + hy);
    }
    draw_clipped (ren, t[1], x, y - add, ex->x3, ex->y3, ex->x4, ex->y3 + pos);
    draw_clipped (ren, t[1], x, y, ex->x3, ex->y3 + pos, ex->x4, ex->y4);
    return;
  }

  if (is_tuple (t, "ver-take", 3) || is_tuple (t, "ver-take", 4)) {
    metric ex;
    get_metric (t[1], ex);
    SI pos= (SI) ((1.0 - as_double (t[2])) * (ex->y2 - ex->y1));
    SI add= (SI) (as_double (t[3]) * (ex->y2 - ex->y1));
    if (is_tuple (t, "ver-take", 4))
      add= (SI) (as_double (t[3]) * as_double (t[4]) * (ex->y2 - ex->y1));
    if (add > 0 && ex->y2 > ex->y1) {
      SI  h = ex->y2 - ex->y1;
      int n = (int) ((20 * add + h - 1) / h);
      SI  dy= (add + n - 1) / n;
      SI  hy= (add + 2*n - 1) / (2*n);
      for (int i=0; i<n; i++)
        draw_clipped (ren, t[1], x, y + i*dy - add - (ex->y3 + pos),
                      ex->x3, ex->y3 + pos - hy, ex->x4, ex->y3 + pos + hy);
    }
    return;
  }

  if (is_tuple (t, "align") && N(t) >= 3) {
    metric ex, ex2;
    get_metric (t[1], ex);
    get_metric (t[2], ex2);
    double xa= 0.0, xa2= 0.0, ya= 0.0, ya2= 0.0;
    if (N(t) >= 4 && is_double (t[3])) xa= xa2= as_double (t[3]);
    if (N(t) >= 5 && is_double (t[4])) ya= ya2= as_double (t[4]);
    if (N(t) >= 6 && is_double (t[5])) xa2= as_double (t[5]);
    if (N(t) >= 7 && is_double (t[6])) ya2= as_double (t[6]);
    SI ax = (SI) (ex ->x1 + xa  * (ex ->x2 - ex ->x1));
    SI ax2= (SI) (ex2->x1 + xa2 * (ex2->x2 - ex2->x1));
    SI ay = (SI) (ex ->y1 + ya  * (ex ->y2 - ex ->y1));
    SI ay2= (SI) (ex2->y1 + ya2 * (ex2->y2 - ex2->y1));
    SI dx = ax2 - ax;
    SI dy = ay2 - ay;
    if (N(t) >= 4 && t[3] == "*") dx= 0;
    if (N(t) >= 5 && t[4] == "*") dy= 0;
    draw_tree (ren, t[1], x+dx, y+dy);
    return;
  }

  if (is_tuple (t, "align*") && N(t) >= 3) {
    metric ex1, ex2;
    get_metric (t[1], ex1);
    get_metric (t[2], ex2);
    double xa1= 0.0, xa2= 0.0, ya1= 0.0, ya2= 0.0;
    if (N(t) >= 4 && is_double (t[3])) xa1= xa2= as_double (t[3]);
    if (N(t) >= 5 && is_double (t[4])) ya1= ya2= as_double (t[4]);
    if (N(t) >= 6 && is_double (t[5])) xa2= as_double (t[5]);
    if (N(t) >= 7 && is_double (t[6])) ya2= as_double (t[6]);
    SI ax1= (SI) (ex1->x3 + xa1 * (ex1->x4 - ex1->x3));
    SI ax2= (SI) (ex2->x3 + xa2 * (ex2->x4 - ex2->x3));
    SI ay1= (SI) (ex1->y3 + ya1 * (ex1->y4 - ex1->y3));
    SI ay2= (SI) (ex2->y3 + ya2 * (ex2->y4 - ex2->y3));
    SI dx = ax2 - ax1;
    SI dy = ay2 - ay1;
    if (N(t) >= 4 && t[3] == "*") dx= 0;
    if (N(t) >= 5 && t[4] == "*") dy= 0;
    draw_tree (ren, t[1], x+dx, y+dy);
    return;
  }

  if (is_tuple (t, "scale", 4) || is_tuple (t, "scale*", 4)) {
    metric ex, ex2;
    get_metric (t[1], ex);
    get_metric (t[2], ex2);
    double sx= 0.0, sy= 0.0;
    if (N(t) >= 4 && t[3] != "*" && is_double (t[3])) sx= as_double (t[3]);
    if (N(t) >= 5 && t[4] != "*" && is_double (t[4])) sy= as_double (t[4]);
    SI w = ex ->x2 - ex ->x1;
    SI w2= ex2->x2 - ex2->x1;
    SI h = ex ->y2 - ex ->y1;
    SI h2= ex2->y2 - ex2->y1;
    if (is_tuple (t, "scale*")) {
      w = ex ->x4 - ex ->x3;
      w2= ex2->x4 - ex2->x3;
      h = ex ->y4 - ex ->y3;
      h2= ex2->y4 - ex2->y3;
    }
    double mx= get_magnification (w, w2, sx);
    double my= get_magnification (h, h2, sy);
    if (N(t) >= 4 && t[3] == "@") sx= sy;
    if (N(t) >= 5 && t[4] == "@") sy= sx;
    ren->move_origin (x, y);
    ren->set_transformation (scaling (point (mx, my), point (0.0, 0.0)));
    draw_tree (ren, t[1], 0, 0);
    ren->reset_transformation ();
    ren->move_origin (-x, -y);
    return;
  }

  if (is_tuple (t, "hor-scale", 2)) {
    scheme_tree ct1= tuple ("hor-crop", t[1]);
    scheme_tree ct2= tuple ("hor-crop", t[2]);
    scheme_tree u  = tuple ("scale", ct1, ct2, "1", "*");
    scheme_tree v  = tuple ("align", u, t[2], "0.5", "0.5");
    draw_tree (ren, v, x, y);
    return;
  }

  if (is_tuple (t, "pretend", 2) ||
      is_tuple (t, "hor-pretend", 2) ||
      is_tuple (t, "left-pretend", 2) ||
      is_tuple (t, "right-pretend", 2) ||
      is_tuple (t, "ver-pretend", 2)) {
    draw_tree (ren, t[1], x, y);
    return;
  }

  if (is_tuple (t, "reslash", 2)) {
    metric ex1, ex2;
    get_metric (t[1], ex1);
    get_metric (t[2], ex2);
    frame fr= reslash (ex1, ex2);
    point p1= fr (point ((double) ex1->x1, (double) ex1->y1));
    point p2= point ((double) ex2->x1, (double) ex2->y1);
    point dp= p2 - p1;
    SI dx= floor (dp[0]);
    SI dy= floor (dp[1]);
    x += dx; y += dy;
    point sh= point ((double) x, (double) y);
    fr= shift_2D (sh) * fr * shift_2D (-sh);
    draw_transformed (ren, t[1], x, y, fr);
    return;
  }

  if (is_tuple (t, "negate", 2)) {
    tree bar= t[2];
    metric ex1, ex2;
    get_metric (t[1], ex1);
    get_metric (t[2], ex2);
    SI h1 = ex1->y4 - ex1->y3;
    SI h2 = ex2->y4 - ex2->y3;
    SI sep= (SI) floor (as_double (exec (tuple ("sep-equal"))) * vunit);
    if (h2 < h1+sep || h2 >= h1+4*sep) {
      double mag= ((double) (h1+2*sep)) / ((double) h2);
      bar= tuple ("magnify", bar, "1", as_string (mag));
    }
    scheme_tree a= tuple ("align", bar, t[1], "0.5", "0.5");
    scheme_tree u= tuple ("join", t[1], a);
    scheme_tree p= tuple ("pretend", u, t[1]);
    draw_tree (ren, p, x, y);
    return;
  }

  if (is_tuple (t, "min-width", 2)) {
    metric ex, ex2;
    get_metric (t[1], ex);
    get_metric (t[2], ex2);
    if ((ex->x2 - ex->x1) <= (ex2->x2 - ex2->x1)) draw_tree (ren, t[1], x, y);
    else draw_tree (ren, t[2], x, y);
  }

  if (is_tuple (t, "max-width", 2)) {
    metric ex, ex2;
    get_metric (t[1], ex);
    get_metric (t[2], ex2);
    if ((ex->x2 - ex->x1) >= (ex2->x2 - ex2->x1)) draw_tree (ren, t[1], x, y);
    else draw_tree (ren, t[2], x, y);
  }

  if (is_tuple (t, "min-height", 2)) {
    metric ex, ex2;
    get_metric (t[1], ex);
    get_metric (t[2], ex2);
    if ((ex->y2 - ex->y1) <= (ex2->y2 - ex2->y1)) draw_tree (ren, t[1], x, y);
    else draw_tree (ren, t[2], x, y);
  }

  if (is_tuple (t, "max-height", 2)) {
    metric ex, ex2;
    get_metric (t[1], ex);
    get_metric (t[2], ex2);
    if ((ex->y2 - ex->y1) >= (ex2->y2 - ex2->y1)) draw_tree (ren, t[1], x, y);
    else draw_tree (ren, t[2], x, y);
  }

  if (is_tuple (t, "font") && N(t) >= 3) {
    draw_tree (ren, t[1], x, y);
    return;
  }

  if (is_tuple (t, "italic", 3)) {
    draw_tree (ren, t[1], x, y);
    return;
  }
}

void
virtual_font_rep::draw_clipped (renderer ren, scheme_tree t, SI x, SI y,
                                SI x1, SI y1, SI x2, SI y2) {
  int bot_extra= ((5 * size + 5) / 10) * PIXEL;
  int top_extra= ((2 * size + 5) / 10) * PIXEL;
  ren->clip (x + x1, y + y1 - bot_extra, x + x2, y + y2 + top_extra);
  draw_tree (ren, t, x, y);
  ren->unclip ();
}

void
virtual_font_rep::draw_transformed (renderer ren, scheme_tree t, SI x, SI y,
                                    frame f) {
  ren->set_transformation (f);
  draw_tree (ren, t, x, y);
  ren->reset_transformation ();  
}

/******************************************************************************
* Getting extents and drawing strings
******************************************************************************/

bool is_hex_digit (char c);

static string
decode_sharp (string c, string& nr) {
  nr= "";
  if (N(c) < 4 || c[N(c)-1] != '>') return c;
  int i= N(c)-2;
  if (c[i] < '0' || c[i] > '9') return c;
  while (i >= 0 && c[i] >= '0' && c[i] <= '9') i--;
  if (i < 0 || c[i] != '-') return c;
  nr= c (i+1, N(c)-1);
  return c (0, i) * "-#>";
}

static tree
subst_sharp (tree t, string by) {
  if (is_atomic (t)) {
    int i;
    string s= t->label;
    i= search_forwards ("#", s);
    if (i == -1) return s;
    else if (i == 0 && N(s) >= 2 && is_hex_digit (s[1])) return s;
    else return s(0,i) * by * s(i+1,N(s));
  }
  else {
    int i, n= N(t);
    tree r (t, n);
    for (i=0; i<n; i++)
      r[i]= subst_sharp (t[i], by);
    return r;
  }
}

static void
make_char_font (string name, font_metric& cfnm, font_glyphs& cfng) {
  cfnm= std_font_metric (name, tm_new_array<metric> (1), 0, 0);
  cfng= std_font_glyphs (name, tm_new_array<glyph> (1), 0, 0);
}

/******************************************************************************
* Getting extents and drawing strings
******************************************************************************/

int
virtual_font_rep::get_char (string s, font_metric& cfnm, font_glyphs& cfng) {
  int n= N(s);
  if (n == 0) return -1;
  if (n == 1) {
    int c= ((QN) s[0]);
    if ((c<0) || (c>=last)) return -1;
    cfnm= fnm;
    cfng= fng;
    if (is_nil (fng->get(c)))
      fng->get(c)= compile (virt->virt_def[c], fnm->get(c));
    return c;
  }
  else if (s[0] == '<' && s[n-1] == '>') {
    if (!virt->dict->contains (s)) {
      if (s[n-2] < '0' || s[n-2] > '9') return -1;
      string nr;
      s= decode_sharp (s, nr);
      if (!virt->dict->contains (s)) return -1;
      int c2= virt->dict [s];
      string ss= "x";
      ss[0]= (char) c2;
      ss << nr;
      return get_char (ss, cfnm, cfng);
    }
    int c2= virt->dict [s];
    cfnm= fnm;
    cfng= fng;
    if (is_nil (fng->get(c2)))
      fng->get(c2)= compile (virt->virt_def[c2], fnm->get(c2));
    return c2;
  }
  else {
    int c= ((QN) s[0]);
    if ((c<0) || (c>=last)) return -1;
    string sub= "[" * as_string (c) * "," * s(1,n) * "]";
    make_char_font (res_name * sub, cfnm, cfng);
    tree t= subst_sharp (virt->virt_def[c], s(1,n));
    if (is_nil (cfng->get(0)))
      cfng->get(0)= compile (t, cfnm->get(0));
    return 0;
  }
}

tree
virtual_font_rep::get_tree (string s) {
  if (s == "") return "";
  int c= ((QN) s[0]), n= N(s);
  if (s[0] == '<' && s[n-1] == '>') {
    if (!virt->dict->contains (s)) {
      if (s[n-2] < '0' || s[n-2] > '9') return "";
      string nr;
      s= decode_sharp (s, nr);
      if (!virt->dict->contains (s)) return "";
      int c2= virt->dict [s];
      return subst_sharp (virt->virt_def[c2], nr);
    }
    int c2= virt->dict [s];
    return virt->virt_def[c2];
  }
  else if ((c<0) || (c>=last)) return "";
  else if (n==1) return virt->virt_def[c];
  else return subst_sharp (virt->virt_def[c], s(1,n));
}

bool
virtual_font_rep::supports (string s) {
  if (extend && base_fn->supports (s)) return true;
  return supported (s, false);
}

void
virtual_font_rep::get_extents (string s, metric& ex) {
  if (extend && base_fn->supports (s)) {
    base_fn->get_extents (s, ex); return; }
  font_metric cfnm;
  font_glyphs cfng;
  int c= get_char (s, cfnm, cfng);
  if (c == -1) {
    ex->y1= y1; ex->y2= y2;
    ex->x1= ex->x2= ex->x3= ex->x4= ex->y3= ex->y4= 0;
  }
  else {
    metric_struct* ey= cfnm->get(c);
    ex->x1= ey->x1; ex->y1= ey->y1;
    ex->x2= ey->x2; ex->y2= ey->y2;
    ex->x3= ey->x3; ex->y3= ey->y3;
    ex->x4= ey->x4; ex->y4= ey->y4;
  }
}

void
virtual_font_rep::get_xpositions (string s, SI* xpos) {
  get_xpositions (s, xpos, 0);
}

void
virtual_font_rep::get_xpositions (string s, SI* xpos, bool lit) {
  (void) lit;
  get_xpositions (s, xpos, 0);
}

void
virtual_font_rep::get_xpositions (string s, SI* xpos, SI xk) {
  if (extend && base_fn->supports (s)) {
    base_fn->get_xpositions (s, xpos, xk); return; }
  metric ex;
  get_extents (s, ex);
  xpos[0]= xk;
  xpos[N(s)]= ex->x2 + xk;
  for (int i=1; i<N(s); i++)
    xpos[i]= (xpos[0] + xpos[N(s)]) >> 1;
}

void
virtual_font_rep::draw_fixed (renderer ren, string s, SI x, SI y) {
  if (extend && base_fn->supports (s))
    base_fn->draw_fixed (ren, s, x, y);
  else if (ren->is_screen || !supported (s, true)) {
    font_metric cfnm;
    font_glyphs cfng;
    int c= get_char (s, cfnm, cfng);
    if (c != -1) ren->draw (c, cfng, x, y);
  }
  else {
    tree t= get_tree (s);
    if (t != "") draw_tree (ren, t, x, y);
  }
}

void
virtual_font_rep::draw_fixed (renderer ren, string s, SI x, SI y, SI xk) {
  draw_fixed (ren, s, x+xk, y);
}

font
virtual_font_rep::magnify (double zoomx, double zoomy) {
  return virtual_font (base_fn->magnify (zoomx, zoomy), fn_name, size,
                       (int) tm_round (hdpi * zoomx),
                       (int) tm_round (vdpi * zoomy), extend);
}

void
virtual_font_rep::advance_glyph (string s, int& pos, bool ligf) {
  pos= N(s);
}

glyph
virtual_font_rep::get_glyph (string s) {
  if (extend && base_fn->supports (s))
    return base_fn->get_glyph (s);
  font_metric cfnm;
  font_glyphs cfng;
  int c= get_char (s, cfnm, cfng);
  if (c == -1) return font_rep::get_glyph (s);
  else return cfng->get(c);
}

int
virtual_font_rep::index_glyph (string s, font_metric& cfnm,
                                         font_glyphs& cfng) {
  if (extend && base_fn->supports (s))
    return base_fn->index_glyph (s, cfnm, cfng);
  return get_char (s, cfnm, cfng);
}

/******************************************************************************
* Slope and italic correction
******************************************************************************/

double
virtual_font_rep::get_left_slope (string s) {
  if (extend && base_fn->supports (s))
    return base_fn->get_left_slope (s);
  tree t= get_tree (s);
  if (is_tuple (t, "italic", 3))
    return as_double (t[2]);
  return font_rep::get_left_slope (s);
}

double
virtual_font_rep::get_right_slope (string s) {
  if (extend && base_fn->supports (s))
    return base_fn->get_right_slope (s);
  tree t= get_tree (s);
  if (is_tuple (t, "italic", 3))
    return as_double (t[2]);
  return font_rep::get_right_slope (s);
}

SI
virtual_font_rep::get_right_correction (string s) {
  if (extend && base_fn->supports (s))
    return base_fn->get_right_correction (s);
  tree t= get_tree (s);
  if (is_tuple (t, "italic", 3))
    return (SI) (as_double (t[3]) * hunit);
  return font_rep::get_right_correction (s);
}

SI
virtual_font_rep::get_lsub_correction (string s) {
  if (extend && base_fn->supports (s))
    return base_fn->get_lsub_correction (s);
  SI r= -get_left_correction (s);
  if (lsub_correct->contains (s)) r += (SI) (lsub_correct[s] * wfn);
  return r;
}

SI
virtual_font_rep::get_lsup_correction (string s) {
  if (extend && base_fn->supports (s))
    return base_fn->get_lsup_correction (s);
  SI r= get_right_correction (s);
  if (lsup_correct->contains (s)) r += (SI) (lsup_correct[s] * wfn);
  return r;
}

SI
virtual_font_rep::get_rsub_correction (string s) {
  if (extend && base_fn->supports (s))
    return base_fn->get_rsub_correction (s);
  if (!rsub_correct->contains (s)) return 0;
  else return (SI) (rsub_correct[s] * wfn);
}

SI
virtual_font_rep::get_rsup_correction (string s) {
  if (extend && base_fn->supports (s))
    return base_fn->get_rsup_correction (s);
  SI r= get_right_correction (s);
  if (rsup_correct->contains (s)) r += (SI) (rsup_correct[s] * wfn);
  return r;
}

SI
virtual_font_rep::get_wide_correction (string s, int mode) {
  if (extend && base_fn->supports (s))
    return base_fn->get_wide_correction (s, mode);
  if (mode > 0 && above_correct->contains (s))
    return (SI) (above_correct[s] * wfn);
  else if (mode < 0 && below_correct->contains (s))
    return (SI) (below_correct[s] * wfn);
  else return 0;
}

/******************************************************************************
* User interface
******************************************************************************/

static hashmap<string,bool> vdefined (false);

bool
virtually_defined (string c, string name) {
  if (!vdefined->contains (name)) {
    vdefined (name)= true;
    translator virt= load_translator (name);
    iterator<string> it= iterate (virt->dict);
    while (it->busy ()) {
      string c= it->next ();
      vdefined (name * "-" * c)= true;
    }
  }
  if (vdefined [name * "-" * c]) return true;
  string nr;
  return vdefined [name * "-" * decode_sharp (c, nr)];
}

font
virtual_font (font base, string name, int size,
              int hdpi, int vdpi, bool extend) {
  string full_name=
    base->res_name * (extend? string ("#enhance-"): string ("#virtual-")) *
    name * as_string (size) * "@" * as_string (hdpi);
  if (vdpi != hdpi) full_name << "x" << vdpi;
  return make (font, full_name,
               tm_new<virtual_font_rep> (full_name, base, name, size,
                                         hdpi, vdpi, extend));
}
