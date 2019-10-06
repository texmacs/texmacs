
/******************************************************************************
* MODULE     : evaluate_length.cpp
* DESCRIPTION: evaluation of lengths
* COPYRIGHT  : (C) 2006  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "evaluate_main.hpp"
#include "vars.hpp"
#include "analyze.hpp"
#include "font.hpp"
#include "gui.hpp"

/******************************************************************************
* Length arithmetic
******************************************************************************/

bool
is_length (string s) {
  int i;
  for (i=0; i<N(s) && !is_locase (s[i]); i++) {}
  return is_double (s (0, i)) && is_locase_alpha (s (i, N(s)));
}

bool
is_length (tree t) {
  return is_atomic (t) && is_length (t->label);
}

bool
is_anylen (tree t) {
  return
    (is_func (t, TMLEN) && ((N(t) == 1) || (N(t) == 3))) ||
    (is_atomic (t) && is_length (t->label)) ||
    is_func (t, MACRO, 1);
}

tree
tmlen_plus (tree t1, tree t2) {
  if ((N(t1) == 1) && (N(t2) == 1))
    return tree (TMLEN, as_string (as_double (t1[0]) + as_double (t2[0])));
  if (N(t1) == 1) t1= tree (TMLEN, t1[0], t1[0], t1[0]);
  if (N(t2) == 1) t2= tree (TMLEN, t2[0], t2[0], t2[0]);
  tree _min= as_string (as_double (t1[0]) + as_double (t2[0]));
  tree _def= as_string (as_double (t1[1]) + as_double (t2[1]));
  tree _max= as_string (as_double (t1[2]) + as_double (t2[2]));
  return tree (TMLEN, _min, _def, _max);
}

tree
tmlen_times (double sc, tree t) {
  if (N(t) == 1) return tree (TMLEN, as_string (sc * as_double (t[0])));
  tree _min= as_string (sc * as_double (t[0]));
  tree _def= as_string (sc * as_double (t[1]));
  tree _max= as_string (sc * as_double (t[2]));
  return tree (TMLEN, _min, _def, _max);
}

tree
tmlen_over (tree t1, tree t2) {
  t1= t1[N(t1)==1? 0: 1];
  t2= t2[N(t2)==1? 0: 1];
  return as_string (as_double (t1) / as_double (t2));
}

/******************************************************************************
* Decoding lengths
******************************************************************************/

tree
as_tmlen (tree t) {
  if (is_func (t, TMLEN)) {
    if (is_double (t[0])) return t;
    if (N(t) < 3) return as_tmlen (t[0]);
    tree _min= as_tmlen (t[0]);
    tree _def= as_tmlen (t[1]);
    tree _max= as_tmlen (t[2]);
    _min= _min[N(_min) == 3? 1: 0];
    _def= _def[N(_def) == 3? 1: 0];
    _max= _max[N(_max) == 3? 1: 0];
    return tree (TMLEN, _min, _def, _max);
  }
  else if (is_atomic (t)) {
    string s= t->label;
    int start= 0, i, n=N(s);
    while ((start+1<n) && (s[start]=='-') && (s[start+1]=='-')) start += 2;
    for (i=start; i<n && !is_locase (s[i]); i++) {}
    string s1= s (start, i);
    string s2= s (i, n);
    if (!(is_double (s1) && is_locase_alpha (s2))) return tree (TMLEN, "0");
    return tmlen_times (as_double (s1),
			as_tmlen (evaluate (compound (s2 * "-length"))));
  }
  else if (is_func (t, MACRO, 1))
    return as_tmlen (evaluate (t[0]));
  else return tree (TMLEN, "0");
}

SI
as_length (string l) {
  tree r= as_tmlen (l);
  string s= r[N(r)==1? 0: 1]->label;
  return (SI) (as_double (s));
}

SI
as_length (tree t) {
  tree r= as_tmlen (t);
  string s= r[N(r)==1? 0: 1]->label;
  return (SI) (as_double (s));
}

/******************************************************************************
* Get environment variables
******************************************************************************/

inline SI std_inch () {
  return (SI) ((double) as_int (std_env [DPI]) * PIXEL); }
inline double std_zoom () {
  return as_double (std_env [ZOOM_FACTOR]); }
inline int std_dpi () {
  return as_int (std_env [DPI]); }
inline double std_magnification () {
  return as_double (std_env [MAGNIFICATION]); }
inline int std_font_base_size () {
  return as_int (std_env [FONT_BASE_SIZE]); }
inline double std_font_size () {
  return as_double (std_env [FONT_SIZE]); }
inline int std_math_level () {
  return as_int (std_env [MATH_LEVEL]); }

inline int std_mode () {
  string s= as_string (std_env [MODE]);
  if (s == "text") return 1;
  else if (s == "math") return 2;
  else if (s == "prog") return 3;
  else return 0; }

font
std_fn () {
  int fs= (int) ((std_font_base_size () + 0.5) * std_font_size ());
  switch (std_mode ()) {
  case 0:
  case 1:
    return smart_font (as_string (std_env [FONT]),
                       as_string (std_env [FONT_FAMILY]),
                       as_string (std_env [FONT_SERIES]),
                       as_string (std_env [FONT_SHAPE]),
                       script (fs, std_math_level ()),
                       (int) (std_magnification () * std_dpi ()));
  case 2:
    return smart_font (as_string (std_env [MATH_FONT]),
                       as_string (std_env [MATH_FONT_FAMILY]),
                       as_string (std_env [MATH_FONT_SERIES]),
                       as_string (std_env [MATH_FONT_SHAPE]),
                       as_string (std_env [FONT]),
                       as_string (std_env [FONT_FAMILY]),
                       as_string (std_env [FONT_SERIES]),
                       "mathitalic",
                       script (fs, std_math_level ()),
                       (int) (std_magnification () * std_dpi ()));
  case 3:
    return smart_font (as_string (std_env [PROG_FONT]),
                       as_string (std_env [PROG_FONT_FAMILY]),
                       as_string (std_env [PROG_FONT_SERIES]),
                       as_string (std_env [PROG_FONT_SHAPE]),
                       as_string (std_env [FONT]),
                       as_string (std_env [FONT_FAMILY]) * "-tt",
                       as_string (std_env [FONT_SERIES]),
                       as_string (std_env [FONT_SHAPE]),
                       script (fs, std_math_level ()),
                       (int) (std_magnification () * std_dpi ()));
  default:
    return get_default_font ();
  }
}

/******************************************************************************
* Universal length units
******************************************************************************/

tree evaluate_cm_length () {
  return tree (TMLEN, as_string (std_inch() / 2.54)); }
tree evaluate_mm_length () {
  return tree (TMLEN, as_string (std_inch() / 25.4)); }
tree evaluate_in_length () {
  return tree (TMLEN, as_string (std_inch())); }
tree evaluate_pt_length () {
  return tree (TMLEN, as_string (std_inch() / 72.27)); }
tree evaluate_bp_length () {
  return tree (TMLEN, as_string (std_inch() / 72.0)); }
tree evaluate_dd_length () {
  return tree (TMLEN, as_string (0.376 * std_inch() / 25.4)); }
tree evaluate_pc_length () {
  return tree (TMLEN, as_string (12.0 * std_inch() / 72.27)); }
tree evaluate_cc_length () {
  return tree (TMLEN, as_string (4.531 * std_inch() / 25.4)); }

/******************************************************************************
* Environment-dependent length units
******************************************************************************/

tree
evaluate_fs_length () {
  double fs= (std_font_base_size () * std_magnification () *
	      std_inch () * std_font_size ()) / 72.0;
  return tree (TMLEN, as_string (fs));
}

tree
evaluate_fbs_length () {
  double fbs= (std_font_base_size () * std_magnification () *
	       std_inch ()) / 72.0;
  return tree (TMLEN, as_string (fbs));
}

tree evaluate_em_length () {
  return tree (TMLEN, as_string (std_fn()->wquad)); }
tree evaluate_ln_length () {
  return tree (TMLEN, as_string (std_fn()->wline)); }
tree evaluate_sep_length () {
  return tree (TMLEN, as_string (std_fn()->sep)); }
tree evaluate_yfrac_length () {
  return tree (TMLEN, as_string (std_fn()->yfrac)); }
tree evaluate_ex_length () {
  return tree (TMLEN, as_string (std_fn()->yx)); }

tree
evaluate_fn_length () {
  double fs= (std_font_base_size () * std_magnification () *
	      std_inch () * std_font_size ()) / 72.0;
  return tree (TMLEN, as_string (0.5*fs), as_string (fs), as_string (1.5*fs));
}

tree
evaluate_fns_length () {
  double fs= (std_font_base_size () * std_magnification () *
	      std_inch () * std_font_size ()) / 72.0;
  return tree (TMLEN, "0", "0", as_string (fs));
}

static space
as_vspace (tree t) {
  tree r= as_tmlen (t);
  if (N(r) == 1)
    return space ((SI) (as_double (r[0]->label)));
  else {
    SI _min= (SI) as_double (r[0]->label);
    SI _def= (SI) as_double (r[1]->label);
    SI _max= (SI) as_double (r[2]->label);
    double flexibility= as_double (std_env [PAGE_FLEXIBILITY]);
    return space (_def + ((SI) (flexibility * (_min - _def))),
		  _def,
		  _def + ((SI) (flexibility * (_max - _def))));
  }
}

tree
evaluate_bls_length () {
  double fs= (std_font_base_size () * std_magnification () *
	      std_inch () * std_font_size ()) / 72.0;
  return tmlen_plus (tree (TMLEN, as_string (fs)),
		     tree (as_vspace (std_env [PAR_SEP])));
}

tree
evaluate_fnbot_length () {
  return tree (TMLEN, as_string (std_fn()->y1));
}

tree
evaluate_fntop_length () {
  return tree (TMLEN, as_string (std_fn()->y2));
}

tree
evaluate_spc_length () {
  space spc= std_fn()->spc;
  return tree (TMLEN,
	       as_string (spc->min),
	       as_string (spc->def),
	       as_string (spc->max));
}

tree
evaluate_xspc_length () {
  space spc= std_fn()->extra;
  return tree (TMLEN,
	       as_string (spc->min),
	       as_string (spc->def),
	       as_string (spc->max));
}

tree
evaluate_par_length () {
  /*
  SI width, d1, d2, d3, d4, d5, d6, d7;
  get_page_pars (width, d1, d2, d3, d4, d5, d6, d7);
  width -= (get_length (PAR_LEFT) + get_length (PAR_RIGHT));
  return tree (TMLEN, as_string (width));
  */
  return tree (TMLEN, as_string (15 * std_inch() / 2.54)); // 15cm
}

tree
evaluate_pag_length () {
  /*
  SI d1, height, d2, d3, d4, d5, d6, d7;
  get_page_pars (d1, height, d2, d3, d4, d5, d6, d7);
  return tree (TMLEN, as_string (height));
  */
  return tree (TMLEN, as_string (23 * std_inch() / 2.54)); // 23cm
}

tree evaluate_tmpt_length () {
  return tree (TMLEN, "1"); }
tree evaluate_px_length () {
  int px= (int) tm_round ((std_shrinkf * PIXEL) / std_zoom ());
  return tree (TMLEN, as_string (px)); }

tree
evaluate_gw_length () {
  //return tree (TMLEN, as_string (gw));
  return tree (TMLEN, as_string (10 * std_inch() / 2.54)); // 10cm
}

tree
evaluate_gh_length () {
  //return tree (TMLEN, as_string (gh));
  return tree (TMLEN, as_string (6 * std_inch() / 2.54)); // 6cm
}

tree
evaluate_gu_length () {
  // FIXME: not yet implemented
  return evaluate_cm_length ();
  //point p0= fr (point (0.0, 0.0));
  //point p1= fr (point (1.0, 0.0));
  //return tree (TMLEN, as_string (norm (p1 - p0)));
}

tree evaluate_msec_length () { return tree (TMLEN, "1"); }
tree evaluate_sec_length () { return tree (TMLEN, "1000"); }
tree evaluate_min_length () { return tree (TMLEN, "60000"); }
tree evaluate_hr_length () { return tree (TMLEN, "3600000"); }
