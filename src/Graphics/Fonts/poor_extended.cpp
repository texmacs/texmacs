
/******************************************************************************
* MODULE     : poor_extended.cpp
* DESCRIPTION: Emulation of extended and condensed fonts
* COPYRIGHT  : (C) 2016  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "font.hpp"
#include "analyze.hpp"
#include "frame.hpp"

/******************************************************************************
* True Type fonts
******************************************************************************/

struct poor_extended_font_rep: font_rep {
  font   base;
  double xf;
  double lw;
  SI     penw;

  poor_extended_font_rep (string name, font base, double xf, double lw);

  bool   supports (string c);
  void   get_extents (string s, metric& ex);
  void   get_xpositions (string s, SI* xpos);
  void   get_xpositions (string s, SI* xpos, bool lig);
  void   get_xpositions (string s, SI* xpos, SI xk);
  void   draw_fixed (renderer ren, string s, SI x, SI y, SI* xpos);
  void   draw_fixed (renderer ren, string s, SI x, SI y);
  void   draw_fixed (renderer ren, string s, SI x, SI y, bool lig);
  void   draw_fixed (renderer ren, string s, SI x, SI y, SI xk);
  font   magnify (double zoomx, double zoomy);
  void   advance_glyph (string s, int& pos);
  glyph  get_glyph (string s);
  int    index_glyph (string s, font_metric& fnm, font_glyphs& fng);
  double get_left_slope  (string s);
  double get_right_slope (string s);
  SI     get_left_correction  (string s);
  SI     get_right_correction  (string s);
  SI     get_lsub_correction  (string s);
  SI     get_lsup_correction  (string s);
  SI     get_rsub_correction  (string s);
  SI     get_rsup_correction  (string s);
  SI     get_wide_correction  (string s, int mode);
};

/******************************************************************************
* Initialization of main font parameters
******************************************************************************/

poor_extended_font_rep::poor_extended_font_rep (
  string name, font b, double f, double l):
    font_rep (name, b), base (b), xf (f), lw (l)
{
  this->copy_math_pars (base);
  this->slope *= xf;
  penw= (SI) floor (lw * wfn + 0.5);
}

/******************************************************************************
* Getting extents and drawing strings
******************************************************************************/

bool
poor_extended_font_rep::supports (string s) {
  return base->supports (s);
}

void
poor_extended_font_rep::get_extents (string s, metric& ex) {
  base->get_extents (s, ex);
  ex->x1= (SI) floor (xf * ex->x1 + 0.5);
  ex->x2= (SI) floor (xf * ex->x2 + 0.5);
  ex->x3= (SI) floor (xf * ex->x3);
  ex->x4= (SI) ceil  (xf * ex->x4);
}

void
poor_extended_font_rep::get_xpositions (string s, SI* xpos) {
  base->get_xpositions (s, xpos);
  for (int i=0; i<N(s); i++)
    xpos[i]= (SI) floor (xpos[i] * xf + 0.5);
}

void
poor_extended_font_rep::get_xpositions (string s, SI* xpos, bool lig) {
  base->get_xpositions (s, xpos, lig);
  for (int i=0; i<N(s); i++)
    xpos[i]= (SI) floor (xpos[i] * xf + 0.5);
}

void
poor_extended_font_rep::get_xpositions (string s, SI* xpos, SI xk) {
  SI xk2= (SI) floor (xk / xf + 0.5);
  base->get_xpositions (s, xpos, xk2);
  for (int i=0; i<N(s); i++)
    xpos[i]= (SI) floor (xpos[i] * xf + 0.5);
}

void
poor_extended_font_rep::draw_fixed (renderer ren, string s,
                                    SI x, SI y, SI* xpos) {
  int i=0;
  while (i < N(s)) {
    int start= i;
    base->advance_glyph (s, i);
    string ss= s (start, i);
    font_metric fnm;
    font_glyphs fng;
    int c= index_glyph (ss, fnm, fng);
    //cout << "Drawing " << ss << ", " << c
    //     << " at " << (xpos[start]/PIXEL) << "\n";
    //cout << fng->get (c) << "\n\n";
    if (c >= 0) ren->draw (c, fng, start==0? x: x + xpos[start], y);
  }
}

void
poor_extended_font_rep::draw_fixed (renderer ren, string s, SI x, SI y) {
  STACK_NEW_ARRAY (xpos, SI, N(s)+1);
  get_xpositions (s, xpos);
  draw_fixed (ren, s, x, y, xpos);
  STACK_DELETE_ARRAY (xpos);
}

void
poor_extended_font_rep::draw_fixed (renderer ren, string s, SI x, SI y, bool l) {
  STACK_NEW_ARRAY (xpos, SI, N(s)+1);
  get_xpositions (s, xpos, l);
  draw_fixed (ren, s, x, y, xpos);
  STACK_DELETE_ARRAY (xpos);
}

void
poor_extended_font_rep::draw_fixed (renderer ren, string s, SI x, SI y, SI xk) {
  STACK_NEW_ARRAY (xpos, SI, N(s)+1);
  get_xpositions (s, xpos, xk);
  draw_fixed (ren, s, x, y, xpos);
  STACK_DELETE_ARRAY (xpos);
}

font
poor_extended_font_rep::magnify (double zoomx, double zoomy) {
  return poor_extended_font (base->magnify (zoomx, zoomy), xf);
}

/******************************************************************************
* Glyph manipulations
******************************************************************************/

void
poor_extended_font_rep::advance_glyph (string s, int& pos) {
  base->advance_glyph (s, pos);
}

glyph
poor_extended_font_rep::get_glyph (string s) {
  glyph gl= base->get_glyph (s);
  return widen (gl, xf, penw);
}

int
poor_extended_font_rep::index_glyph (string s, font_metric& fnm,
                                               font_glyphs& fng) {
  int c= base->index_glyph (s, fnm, fng);
  if (c < 0) return c;
  fnm= stretched (fnm, xf, 1.0);
  fng= extended (fng, xf, penw);
  return c;
}

/******************************************************************************
* Extended correction
******************************************************************************/

double
poor_extended_font_rep::get_left_slope (string s) {
  return base->get_left_slope (s) * xf;
}

double
poor_extended_font_rep::get_right_slope (string s) {
  return base->get_right_slope (s) * xf;
}

SI
poor_extended_font_rep::get_left_correction (string s) {
  return (SI) floor (base->get_left_correction (s) * xf + 0.5);
}

SI
poor_extended_font_rep::get_right_correction (string s) {
  return (SI) floor (base->get_right_correction (s) * xf + 0.5);
}

SI
poor_extended_font_rep::get_lsub_correction (string s) {
  return (SI) floor (base->get_lsub_correction (s) * xf + 0.5);
}

SI
poor_extended_font_rep::get_lsup_correction (string s) {
  return (SI) floor (base->get_lsup_correction (s) * xf + 0.5);
}

SI
poor_extended_font_rep::get_rsub_correction (string s) {
  return (SI) floor (base->get_rsub_correction (s) * xf + 0.5);
}

SI
poor_extended_font_rep::get_rsup_correction (string s) {
  return (SI) floor (base->get_rsup_correction (s) * xf + 0.5);
}

SI
poor_extended_font_rep::get_wide_correction (string s, int mode) {
  return (SI) floor (base->get_wide_correction (s, mode) * xf + 0.5);
}

/******************************************************************************
* Interface
******************************************************************************/

font
poor_extended_font (font base, double xf, double lw) {
  if (xf == 1.0) return base;
  string name= "poorextended[" * base->res_name;
  name << "," << as_string (xf) << "," << as_string (lw) << "]";
  return make (font, name,
               tm_new<poor_extended_font_rep> (name, base, xf, lw));
}

static int
get_o_pen_width (glyph o) {
  int ww= o->width, hh= o->height;
  int x1= probe (o, 0, hh>>1, 1, 0);
  int x2= probe (o, ww>>1, hh>>1, -1, 0);
  return x2 + 1 - x1;
}

font
poor_extended_font (font base, double xf) {
  glyph  o  = base->get_glyph ("o");
  glyph  O  = base->get_glyph ("O");
  SI     pwo= get_o_pen_width (o);
  SI     pwO= get_o_pen_width (O);
  double fo = ((double) pwo) / ((double) o->width);
  double fO = ((double) pwO) / ((double) O->width);
  metric eo; base->get_extents ("o", eo);
  metric eO; base->get_extents ("O", eO);
  double efo= ((double) (eo->x4 - eo->x3)) / ((double) base->wfn);
  double efO= ((double) (eO->x4 - eO->x3)) / ((double) base->wfn);
  double ro = fo * efo;
  double rO = fO * efO;
  double lw = sqrt (ro * rO);
  return poor_extended_font (base, xf, lw);
}
