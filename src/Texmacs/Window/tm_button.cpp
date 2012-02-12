
/******************************************************************************
* MODULE     : tm_button.cpp
* DESCRIPTION: Text widgets for output only
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "boxes.hpp"
#include "Boxes/construct.hpp"
#include "font.hpp"
#include "tm_frame.hpp"
#include "message.hpp"
#ifdef AQUATEXMACS
#include "Cocoa/aqua_simple_widget.h"
#else
#ifdef QTTEXMACS
#include "Qt/qt_simple_widget.hpp"
#else
#include "Widkit/simple_wk_widget.hpp"
#endif
#endif

/******************************************************************************
* Getting extents of a typesetted tree
* Application: get window size for widget tree
******************************************************************************/

#include "gui.hpp"
#include "drd_std.hpp"
#include "drd_info.hpp"
#include "convert.hpp"
#include "formatter.hpp"
#include "Format/format.hpp"

void use_modules (tree t);

edit_env
get_init_environment (tree doc, drd_info& drd) {
  hashmap<string,tree> h1 (UNINIT), h2 (UNINIT), h3 (UNINIT), h4 (UNINIT);
  edit_env env (drd, "none", h1, h2, h3, h4);
  env->write_default_env ();
  bool ok;
  tree t, style= extract (doc, "style");
  hashmap<string,tree> H;
  get_server () -> style_get_cache (style, H, t, ok);
  if (ok) {
    env->patch_env (H);
    ok= drd->set_locals (t);
    drd->set_environment (H);
  }
  if (!ok) {
    ASSERT (is_tuple (style), "tuple expected as style");
    tree t (USE_PACKAGE, A (style));
    env->exec (t);
    env->read_env (H);
    drd->heuristic_init (H);
  }
  use_modules (env->read (THE_MODULES));
  // FIXME: extract (doc, "init")
  // env->write (PAGE_TYPE, "a5");
  env->update ();
  return env;
}

tree
tree_extents (tree doc) {
  drd_info drd ("none", std_drd);
  edit_env env= get_init_environment (doc, drd);
  tree t= extract (doc, "body");
  lazy lz= make_lazy (env, t, path ());
  format vf= make_query_vstream_width (array<line_item>(), array<line_item>());
  format rf= lz->query (LAZY_BOX, vf);
  SI w= ((format_vstream) rf)->width;
  box b= (box) lz->produce (LAZY_BOX, make_format_width (w));
  SI h= b->h ();
  w += env->get_length (PAGE_SCREEN_LEFT);
  w += env->get_length (PAGE_SCREEN_RIGHT);
  h += env->get_length (PAGE_SCREEN_TOP);
  h += env->get_length (PAGE_SCREEN_BOT);
  return tuple (as_tree ((w / (5*PIXEL)) + 1), as_tree ((h / (5*PIXEL)) + 1));
}

/******************************************************************************
* Typesetted boxes as widgets
******************************************************************************/

class box_widget_rep: public simple_widget_rep {
  box   b;
  color bg;
  bool  transparent;
  int   sf;
  int   dw, dh;

public:
  box_widget_rep (box b, color bg, bool trans, int sf, int dw, int dh);
  operator tree ();

  void handle_get_size_hint (SI& w, SI& h);
  void handle_repaint (SI x1, SI y1, SI x2, SI y2);
};

box_widget_rep::box_widget_rep
  (box b2, color bg2, bool trans2, int sf2, int dw2, int dh2):
    simple_widget_rep (), b (b2),
    bg (bg2), transparent (trans2), sf (sf2),
    dw (dw2+2*PIXEL), dh (dh2+2*PIXEL) {}

box_widget_rep::operator tree () {
  return tree (TUPLE, "box", (tree) b);
}

void
box_widget_rep::handle_get_size_hint (SI& w, SI& h) {
  SI X1= b->x1, Y1= b->y1;
  SI X2= b->x2, Y2= b->y2;
  w = ((X2- X1+ sf- 1)/sf)+ 2*dw;
  h = ((Y2- Y1+ sf- 1)/sf)+ 2*dh;
  abs_round (w, h);
}

void
box_widget_rep::handle_repaint (SI x1, SI y1, SI x2, SI y2) {
  SI w, h;
  handle_get_size_hint (w, h);
  renderer ren= get_renderer (this);
  if (!transparent) {
    ren->set_background (bg);
    ren->set_color (bg);
    ren->fill (x1, y1, x2, y2);
  }
  ren->set_shrinking_factor (sf);
  rectangles l (rectangle (0, 0, w, h));
  SI x= ((sf*w-b->w())>>1) - b->x1;
  SI y= ((sf*h-b->h())>>1) - b->y1 - sf*h;
  b->redraw (ren, path(), l, x, y);
  ren->set_shrinking_factor (1);
}

/******************************************************************************
* Interface
******************************************************************************/

widget
box_widget (box b, bool tr) {
  color col= light_grey;
  return widget (tm_new<box_widget_rep> (b, col, tr, 6, 3*PIXEL, 3*PIXEL));
}

widget
box_widget (scheme_tree p, string s, color col, bool trans, bool ink) {
  string family  = "roman";
  string fn_class= "mr";
  string series  = "medium";
  string shape   = "normal";
  int    sz      = 10;
  int    dpi     = 600;
  int    n       = arity (p);
  if ((n >= 1) && is_atomic (p[0])) family  = as_string (p[0]);
  if ((n >= 2) && is_atomic (p[1])) fn_class= as_string (p[1]);
  if ((n >= 3) && is_atomic (p[2])) series  = as_string (p[2]);
  if ((n >= 4) && is_atomic (p[3])) shape   = as_string (p[3]);
  if ((n >= 5) && is_atomic (p[4])) sz      = as_int (p[4]);
  if ((n >= 6) && is_atomic (p[5])) dpi     = as_int (p[5]);
  font fn= find_font (family, fn_class, series, shape, sz, dpi);
  box  b = text_box (decorate (), 0, s, fn, col);
  if (ink) b= resize_box (decorate (), b, b->x3, b->y3, b->x4, b->y4, true);
  return box_widget (b, trans);
}

tree enrich_embedded_document (tree body);

widget
texmacs_output_widget (tree doc) {
  doc= enrich_embedded_document (doc);
  drd_info drd ("none", std_drd);
  edit_env env= get_init_environment (doc, drd);
  tree t= extract (doc, "body");
  lazy lz= make_lazy (env, t, path ());
  format vf= make_query_vstream_width (array<line_item>(), array<line_item>());
  format rf= lz->query (LAZY_BOX, vf);
  SI w= ((format_vstream) rf)->width;
  box b= (box) lz->produce (LAZY_BOX, make_format_width (w));
  //cout << (b->w()>>8) << ", " << (b->h()>>8) << "\n";
  //SI dw1= env->get_length (PAGE_SCREEN_LEFT);
  //SI dw2= env->get_length (PAGE_SCREEN_RIGHT);
  //SI dh1= env->get_length (PAGE_SCREEN_BOT);
  //SI dh2= env->get_length (PAGE_SCREEN_TOP);
  color col= light_grey;
  return widget (tm_new<box_widget_rep> (b, col, false, 5, 0, 0));
}
