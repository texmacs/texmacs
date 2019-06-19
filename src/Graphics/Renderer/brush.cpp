
/******************************************************************************
* MODULE     : brush.cpp
* DESCRIPTION: brushes for painting
* COPYRIGHT  : (C) 2013  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "renderer.hpp"
#include "gui.hpp"
#include "image_files.hpp"
#include "true_color.hpp"

url get_current_buffer_safe ();
bool is_percentage (tree t, string s= "%");
double as_percentage (tree t);

/******************************************************************************
* Equality
******************************************************************************/

bool
operator == (const brush& a, const brush& b) {
  brush_rep* ar= (brush_rep*) a.rep;
  brush_rep* br= (brush_rep*) b.rep;
  return
    ar->get_type () == br->get_type () &&
    ar->get_color () == br->get_color () &&
    ar->get_pattern () == br->get_pattern () &&
    ar->get_alpha () == br->get_alpha ();
}

/******************************************************************************
* No brush
******************************************************************************/

class no_brush_rep: public brush_rep {
public:
  no_brush_rep () {}

  brush_kind get_type () { return brush_none; }
  void* get_handle () { return (void*) this; }

  color get_color () { return rgb_color (0, 0, 0, 0); }
  tree get_pattern () { return ""; }
  int get_alpha () { return 0; }
};

/******************************************************************************
* Monochrome brushes
******************************************************************************/

class color_brush_rep: public brush_rep {
  color c;

public:
  color_brush_rep (color c2): c (c2) {}

  brush_kind get_type () { return brush_color; }
  void* get_handle () { return (void*) this; }

  color get_color () { return c; }
  tree get_pattern () { return ""; }
  int get_alpha () { return 255; }
};

/******************************************************************************
* Patterns
******************************************************************************/

class pattern_brush_rep: public brush_rep {
  color c;
  tree pattern;
  int alpha;

public:
  pattern_brush_rep (color c2, tree p, int a):
    c (c2), pattern (p), alpha (a) {}

  brush_kind get_type () { return brush_pattern; }
  void* get_handle () { return (void*) this; }

  color get_color () { return c; }
  tree get_pattern () { return pattern; }
  int get_alpha () { return alpha; }
};

url
brush_rep::get_pattern_url () {
  tree t= get_pattern ();
  if (is_atomic (t) || N(t) == 0 || !is_atomic (t[0])) return url ();
  url u= url_system (as_string (t[0]));
  url r= resolve (url ("$TEXMACS_PATTERN_PATH") * u);
  if (!is_none (r)) return r;
  url base= get_current_buffer_safe ();
  r= resolve (relative (base, u));
  if (!is_none (r)) return r;
  return u;
}

/******************************************************************************
* Constructors
******************************************************************************/

static brush_rep*
make_brush (bool b) {
  if (b) return tm_new<color_brush_rep> (white);
  else return tm_new<no_brush_rep> ();
}

static brush_rep*
make_brush (color c) {
  int r, g, b, a;
  get_rgb_color (c, r, g, b, a);
  if (a == 0) return tm_new<no_brush_rep> ();
  else return tm_new<color_brush_rep> (c);
}

static brush_rep*
make_brush (tree p, int a) {
  if (is_atomic (p)) {
    string s= p->label;
    if (s == "" || s == "none")
      return tm_new<no_brush_rep> ();
    else
      return make_brush (named_color (s, a));
  }
  else {
    color c= white;
    if (N(p) == 4) c= named_color (as_string (p[3]), a);
    return tm_new<pattern_brush_rep> (c, p, a);
  }
}

//brush::brush (): rep (make_brush (white)) { INC_COUNT (rep); }
brush::brush (color c): rep (make_brush (c)) { INC_COUNT (rep); }
brush::brush (bool b): rep (make_brush (b)) { INC_COUNT (rep); }
brush::brush (tree p, int a): rep (make_brush (p, a)) { INC_COUNT (rep); }

brush
mix (brush b1, double a1, brush b2, double a2) {
  if (b1 == b2) return b1;
  if (b1->get_type () == brush_pattern) return b1;
  if (b2->get_type () == brush_pattern) return b2;
  true_color c1= true_color (b1->get_color ());
  true_color c2= true_color (b2->get_color ());
  true_color mc= mix (c1, a1, c2, a2);
  return brush ((color) mc);
}

/******************************************************************************
* Unpacking pattern data
******************************************************************************/

void
get_pattern_data (url& u, SI& w, SI& h, tree& eff, brush br, SI pixel) {
  // FIXME << what's about ratio and percentages wrt paper lengths?
  tree pattern= br->get_pattern ();
  u= br->get_pattern_url ();
  int imw_pt, imh_pt;
  image_size (u, imw_pt, imh_pt);
  double pt= ((double) 600*PIXEL) / 72.0;
  SI imw= (SI) (((double) imw_pt) * pt);
  SI imh= (SI) (((double) imh_pt) * pt);
  w= imw, h= imh;
  if (is_int (pattern[1])) w= as_int (pattern[1]);
  else if (is_percentage (pattern[1]))
    w= (SI) (as_percentage (pattern[1]) * ((double) w));
  else if (is_percentage (pattern[1], "@"))
    w= (SI) (as_percentage (pattern[1]) * ((double) h));
  if (is_int (pattern[2])) h= as_int (pattern[2]);
  else if (is_percentage (pattern[2]))
    h= (SI) (as_percentage (pattern[2]) * ((double) h));
  else if (is_percentage (pattern[2], "@"))
    h= (SI) (as_percentage (pattern[2]) * ((double) w));
  w= ((w + pixel - 1) / pixel);
  h= ((h + pixel - 1) / pixel);
  eff= (N(pattern) >= 4 && is_compound (pattern[3])? pattern[3]: tree (""));
}
