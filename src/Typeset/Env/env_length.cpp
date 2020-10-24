
/******************************************************************************
* MODULE     : env_length.cpp
* DESCRIPTION: length computations
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "analyze.hpp"
#include "env.hpp"
#include "convert.hpp"
#include "page_type.hpp"
#include "typesetter.hpp"
#include "Boxes/construct.hpp"

/******************************************************************************
* Length arithmetic
******************************************************************************/

bool
edit_env_rep::is_length (string s) {
  int i;
  for (i=0; (i<N(s)) && (!is_locase (s[i])); i++) {}
  return is_double (s (0, i)) && is_locase_alpha (s (i, N(s)));
}

bool
edit_env_rep::is_anylen (tree t) {
  return
    (is_func (t, TMLEN) && ((N(t) == 1) || (N(t) == 3))) ||
    (is_atomic (t) && is_length (t->label)) ||
    is_func (t, MACRO, 1);
}

tree
edit_env_rep::tmlen_plus (tree t1, tree t2) {
  if ((N(t1) == 1) && (N(t2) == 1))
    return tree (TMLEN, as_string (as_double (t1[0]) + as_double (t2[0])));
  if (N(t1) == 1) t1= tree (TMLEN, t1[0], t1[0], t1[0]);
  if (N(t2) == 1) t2= tree (TMLEN, t2[0], t2[0], t2[0]);
  if (N(t1) < 3 || N(t2) < 3) return tree (ERROR, "invalid tmlen-plus");
  tree _min= as_string (as_double (t1[0]) + as_double (t2[0]));
  tree _def= as_string (as_double (t1[1]) + as_double (t2[1]));
  tree _max= as_string (as_double (t1[2]) + as_double (t2[2]));
  return tree (TMLEN, _min, _def, _max);
}

tree
edit_env_rep::tmlen_min (tree t1, tree t2) {
  if ((N(t1) == 1) && (N(t2) == 1))
    return tree (TMLEN, as_string (min (as_double(t1[0]), as_double(t2[0]))));
  if (N(t1) == 1) t1= tree (TMLEN, t1[0], t1[0], t1[0]);
  if (N(t2) == 1) t2= tree (TMLEN, t2[0], t2[0], t2[0]);
  if (N(t1) < 3 || N(t2) < 3) return tree (ERROR, "invalid tmlen-plus");
  tree _min= as_string (min (as_double (t1[0]), as_double (t2[0])));
  tree _def= as_string (min (as_double (t1[1]), as_double (t2[1])));
  tree _max= as_string (min (as_double (t1[2]), as_double (t2[2])));
  return tree (TMLEN, _min, _def, _max);
}

tree
edit_env_rep::tmlen_max (tree t1, tree t2) {
  if ((N(t1) == 1) && (N(t2) == 1))
    return tree (TMLEN, as_string (max (as_double(t1[0]), as_double(t2[0]))));
  if (N(t1) == 1) t1= tree (TMLEN, t1[0], t1[0], t1[0]);
  if (N(t2) == 1) t2= tree (TMLEN, t2[0], t2[0], t2[0]);
  if (N(t1) < 3 || N(t2) < 3) return tree (ERROR, "invalid tmlen-plus");
  tree _min= as_string (max (as_double (t1[0]), as_double (t2[0])));
  tree _def= as_string (max (as_double (t1[1]), as_double (t2[1])));
  tree _max= as_string (max (as_double (t1[2]), as_double (t2[2])));
  return tree (TMLEN, _min, _def, _max);
}

tree
edit_env_rep::tmlen_times (double sc, tree t) {
  if (N(t) == 1) return tree (TMLEN, as_string (sc * as_double (t[0])));
  if (N(t) < 3) return tree (ERROR, "invalid tmlen-times");
  tree _min= as_string (sc * as_double (t[0]));
  tree _def= as_string (sc * as_double (t[1]));
  tree _max= as_string (sc * as_double (t[2]));
  return tree (TMLEN, _min, _def, _max);
}

tree
edit_env_rep::tmlen_over (tree t1, tree t2) {
  t1= t1[N(t1)==1? 0: 1];
  t2= t2[N(t2)==1? 0: 1];
  return as_string (as_double (t1) / as_double (t2));
}

double
edit_env_rep::tmlen_div (tree t1, tree t2) {
  t1= t1[N(t1)==1? 0: 1];
  t2= t2[N(t2)==1? 0: 1];
  return floor ((as_double (t1) / as_double (t2)) + 0.0001);
}

tree
edit_env_rep::tmlen_mod (tree t1, tree t2) {
  double div= tmlen_div (t1, t2);
  return tmlen_plus (t1, tmlen_times (-div, t2));
}

/******************************************************************************
* Length arithmetic for strings
******************************************************************************/

void
edit_env_rep::get_length_unit (string s, SI& un, string& un_str) {
  int i;
  for (i=0; i<N(s); i++)
    if (is_locase (s[i])) break;
  un= as_length (string ("1" * s (i, N(s))));
  un_str= s (i, N(s));
}

string
edit_env_rep::add_lengths (string s1, string s2) {
  SI l1= as_length (s1);
  SI l2= as_length (s2);
  SI un; string un_str;
  get_length_unit (s1, un, un_str);
  if (un==0) return "0tmpt";
  double x= ((double) (l1+l2)) / ((double) un);
  return as_string (x) * un_str;
}

string
edit_env_rep::sub_lengths (string s1, string s2) {
  SI l1= as_length (s1);
  SI l2= as_length (s2);
  SI un; string un_str;
  get_length_unit (s1, un, un_str);
  if (un==0) return "0tmpt";
  double x= ((double) (l1-l2)) / ((double) un);
  return as_string (x) * un_str;
}

string
edit_env_rep::max_lengths (string s1, string s2) {
  SI l1= as_length (s1);
  SI l2= as_length (s2);
  return (l1 >= l2? s1 : s2);
}

string
edit_env_rep::min_lengths (string s1, string s2) {
  SI l1= as_length (s1);
  SI l2= as_length (s2);
  return (l1 <= l2? s1 : s2);
}

string
edit_env_rep::multiply_length (double x, string s) {
  SI l= as_length (s);
  SI un; string un_str;
  get_length_unit (s, un, un_str);
  if (un==0) return "0tmpt";
  double xl= (x*l) / ((double) un);
  return as_string (xl) * un_str;
}

double
edit_env_rep::divide_lengths (string s1, string s2) {
  SI l1= as_length (s1);
  SI l2= as_length (s2);
  return ((double) l1) / ((double) l2);
}

/******************************************************************************
* Decoding lengths
******************************************************************************/

tree
edit_env_rep::as_tmlen (tree t) {
  if (is_func (t, TMLEN)) {
    if (N(t) == 0) return t;
    if (is_double (t[0])) return t;
    if (N(t) < 3) return as_tmlen (t[0]);
    tree _min= as_tmlen (t[0]);
    tree _def= as_tmlen (t[1]);
    tree _max= as_tmlen (t[2]);
    if (N(_min) < 1) return t;
    if (N(_def) < 1) return t;
    if (N(_max) < 1) return t;
    _min= _min[N(_min) == 3? 1: 0];
    _def= _def[N(_def) == 3? 1: 0];
    _max= _max[N(_max) == 3? 1: 0];
    return tree (TMLEN, _min, _def, _max);
  }
  else if (is_atomic (t)) {
    string s= t->label;
    int start= 0, n=N(s);
    while ((start+1<n) && (s[start]=='-') && (s[start+1]=='-')) start += 2;
    double len;
    string unit;
    parse_length (s (start, n), len, unit);
    if (unit == "error" || is_empty (unit)) {
      return tree (TMLEN, "0");
    } else {
      return tmlen_times (len, as_tmlen (exec (compound (unit * "-length"))));
    }
  }
  else if (is_func (t, MACRO, 1))
    return as_tmlen (exec (t[0]));
  else return tree (TMLEN, "0");
}

SI
edit_env_rep::as_length (tree t) {
  tree r= as_tmlen (t);
  if (N(r) < 1) return 0;
  string s= r[N(r)==1? 0: 1]->label;
  return (SI) (as_double (s));
}

SI
edit_env_rep::as_length (tree t, string perc) {
  if (is_atomic (t) && N(t->label) > 0 && t->label [N(t->label) - 1] == '%')
    return as_length (t->label (0, N(t->label) - 1) * perc) / 100;
  else {
    tree r= as_tmlen (t);
    if (N(r) < 1) return 0;
    string s= r[N(r)==1? 0: 1]->label;
    return (SI) (as_double (s));
  }
}

SI
edit_env_rep::as_eff_length (tree t) {
  if (is_int (t)) return as_int (t);
  else return as_length (t);
}

space
edit_env_rep::as_hspace (tree t) {
  tree r= as_tmlen (t);
  if (N(r) == 1)
    return space ((SI) (as_double (r[0]->label)));
  else if (N(r) < 3)
    return 0;
  else {
    SI _min= (SI) as_double (r[0]->label);
    SI _def= (SI) as_double (r[1]->label);
    SI _max= (SI) as_double (r[2]->label);
    return space (_min, _def, _max);
  }
}

space
edit_env_rep::as_vspace (tree t) {
  tree r= as_tmlen (t);
  if (N(r) == 1)
    return space ((SI) (as_double (r[0]->label)));
  else if (N(r) < 3)
    return 0;
  else {
    SI _min= (SI) as_double (r[0]->label);
    SI _def= (SI) as_double (r[1]->label);
    SI _max= (SI) as_double (r[2]->label);
    return space (_def + ((SI) (flexibility * (_min - _def))),
		  _def,
		  _def + ((SI) (flexibility * (_max - _def))));
  }
}

point
edit_env_rep::as_point (tree t) {
  if ((is_tuple (t) || is_point (t)) && ((N(t)==0) || is_double (t[0])))
    return ::as_point (t);
  if (is_tuple (t) || is_point (t)) {
    int i, n= N(t);
    point p(n);
    for (i=0; i<n; i++)
      p[i]= as_length (t[i]);
    return fr[p];
  }
  if (is_func (t, WITH)) {
    for (int i=0; i<N(t)-1; i+=2) {
      tree var= t[i+1];
      if (is_func (var, QUOTE, 1)) var= var[0];
      if (t[i] == GID && has_graphical_value (var)) {
        tree old_t= t[N(t)-1];
        tree new_t= get_graphical_value (var);
        if (new_t != old_t) {
          point old_p= as_point (old_t);
          point new_p= as_point (new_t);
          if (new_p != old_p) {
            graphics_require_update (var);
            return old_p;//return new_p;
          }
        }
      }
    }
    return as_point (t[N(t)-1]);
  }
  return point ();
}

/******************************************************************************
* Percentages and magnifications
******************************************************************************/

bool
is_percentage (tree t, string s) {
  return
    is_atomic (t) &&
    ends (t->label, s) &&
    is_double (t->label (0, N (t->label) - 1));
}

bool
is_percentage (tree t) {
  return is_percentage (t, "%");
}

double
as_percentage (tree t) {
  return as_double (t->label (0, N (t->label) - 1)) / 100.0;
}

bool
is_magnification (string s) {
  double result;
  if (N(s) == 0) return false;
  for (int i=0; i<N(s); /*nop*/) {
    if (s[i]=='*') { i++; read_double (s, i, result); }
    else if (s[i]=='/') { i++; read_double (s, i, result); }
    else return false;
  }
  return true;
}

double
get_magnification (string s) {
  int i=0;
  double magn= 1.0, result;
  while (i<N(s)) {
    if (s[i]=='*') { i++; read_double (s, i, result); magn *= result; }
    else if (s[i]=='/') { i++; read_double (s, i, result); magn /= result; }
    else return magn;
  }
  return magn;
}

/******************************************************************************
* Built-in length units
******************************************************************************/

tree edit_env_rep::exec_cm_length () {
  return tree (TMLEN, as_string (magn_len*inch/2.54)); }
tree edit_env_rep::exec_mm_length () {
  return tree (TMLEN, as_string (magn_len*inch/25.4)); }
tree edit_env_rep::exec_in_length () {
  return tree (TMLEN, as_string (magn_len*inch)); }
tree edit_env_rep::exec_pt_length () {
  return tree (TMLEN, as_string (magn_len*inch/72.27)); }
tree edit_env_rep::exec_bp_length () {
  return tree (TMLEN, as_string (magn_len*inch/72.0)); }
tree edit_env_rep::exec_dd_length () {
  return tree (TMLEN, as_string (0.376*magn_len*inch/25.4)); }
tree edit_env_rep::exec_pc_length () {
  return tree (TMLEN, as_string (12.0*magn_len*inch/72.27)); }
tree edit_env_rep::exec_cc_length () {
  return tree (TMLEN, as_string (4.531*magn_len*inch/25.4)); }

tree
edit_env_rep::exec_fs_length () {
  double fs=
    (get_int(FONT_BASE_SIZE) * magn * inch * get_double(FONT_SIZE)) / 72.0;
  return tree (TMLEN, as_string (fs));
}

tree
edit_env_rep::exec_fbs_length () {
  double fbs= (get_int(FONT_BASE_SIZE) * magn * inch) / 72.0;
  return tree (TMLEN, as_string (fbs));
}

tree edit_env_rep::exec_em_length () {
  return tree (TMLEN, as_string (fn->wquad)); }
tree edit_env_rep::exec_ln_length () {
  return tree (TMLEN, as_string (fn->wline)); }
tree edit_env_rep::exec_sep_length () {
  return tree (TMLEN, as_string (fn->sep)); }
tree edit_env_rep::exec_yfrac_length () {
  return tree (TMLEN, as_string (fn->yfrac)); }
tree edit_env_rep::exec_ex_length () {
  return tree (TMLEN, as_string (fn->yx)); }

tree
edit_env_rep::exec_fn_length () {
  double fn=
    (get_int(FONT_BASE_SIZE) * magn * inch * get_double(FONT_SIZE)) / 72.0;
  return tree (TMLEN, as_string (0.5*fn), as_string (fn), as_string (1.5*fn));
}

tree
edit_env_rep::exec_fns_length () {
  double fn=
    (get_int(FONT_BASE_SIZE) * magn * inch * get_double(FONT_SIZE)) / 72.0;
  return tree (TMLEN, "0", "0", as_string (fn));
}

tree
edit_env_rep::exec_bls_length () {
  double fn=
    (get_int(FONT_BASE_SIZE) * magn * inch * get_double(FONT_SIZE)) / 72.0;
  return tmlen_plus (tree (TMLEN, as_string (fn)), get_vspace (PAR_SEP));
}

tree
edit_env_rep::exec_fnbot_length () {
  return tree (TMLEN, as_string (fn->y1));
}

tree
edit_env_rep::exec_fntop_length () {
  return tree (TMLEN, as_string (fn->y2));
}

tree
edit_env_rep::exec_spc_length () {
  return tree (TMLEN,
	       as_string (fn->spc->min),
	       as_string (fn->spc->def),
	       as_string (fn->spc->max));
}

tree
edit_env_rep::exec_xspc_length () {
  return tree (TMLEN,
	       as_string (fn->extra->min),
	       as_string (fn->extra->def),
	       as_string (fn->extra->max));
}

tree
edit_env_rep::exec_par_length () {
  SI width, d1, d2, d3, d4, d5, d6, d7;
  if (read (PAR_WIDTH) != "auto") {
    width= get_length (PAR_WIDTH);
    int nr_cols= get_int (PAR_COLUMNS);
    if (nr_cols > 1) {
      SI col_sep= get_length (PAR_COLUMNS_SEP);
      width= ((width+col_sep) / nr_cols) - col_sep;
    }
  }
  else get_page_pars (width, d1, d2, d3, d4, d5, d6, d7);
  width -= (get_length (PAR_LEFT) + get_length (PAR_RIGHT));
  return tree (TMLEN, as_string (width));
}

tree
edit_env_rep::exec_paw_length () {
  SI width, d1, d2, d3, d4, d5, d6, d7;
  get_page_pars (width, d1, d2, d3, d4, d5, d6, d7);
  return tree (TMLEN, as_string (width));
}

tree
edit_env_rep::exec_pag_length () {
  SI d1, height, d2, d3, d4, d5, d6, d7;
  get_page_pars (d1, height, d2, d3, d4, d5, d6, d7);
  return tree (TMLEN, as_string (height));
}

tree edit_env_rep::exec_tmpt_length () {
  return tree (TMLEN, "1"); }
tree edit_env_rep::exec_px_length () {
  return tree (TMLEN, as_string (pixel)); }

tree edit_env_rep::exec_lcorner_length () {
  ornament_parameters ps= get_ornament_parameters ();
  return tree (TMLEN, as_string (ps->lcor)); }
tree edit_env_rep::exec_bcorner_length () {
  ornament_parameters ps= get_ornament_parameters ();
  return tree (TMLEN, as_string (ps->bcor)); }
tree edit_env_rep::exec_rcorner_length () {
  ornament_parameters ps= get_ornament_parameters ();
  return tree (TMLEN, as_string (ps->rcor)); }
tree edit_env_rep::exec_tcorner_length () {
  ornament_parameters ps= get_ornament_parameters ();
  return tree (TMLEN, as_string (ps->tcor)); }

tree edit_env_rep::exec_gw_length () {
  return tree (TMLEN, as_string (gw)); }
tree edit_env_rep::exec_gh_length () {
  return tree (TMLEN, as_string (gh)); }
tree edit_env_rep::exec_gu_length () {
  point p0= fr (point (0.0, 0.0));
  point p1= fr (point (1.0, 0.0));
  return tree (TMLEN, as_string (norm (p1 - p0))); }

tree edit_env_rep::exec_ms_length () { return tree (TMLEN, "1"); }
tree edit_env_rep::exec_s_length () { return tree (TMLEN, "1000"); }
tree edit_env_rep::exec_msec_length () { return tree (TMLEN, "1"); }
tree edit_env_rep::exec_sec_length () { return tree (TMLEN, "1000"); }
tree edit_env_rep::exec_min_length () { return tree (TMLEN, "60000"); }
tree edit_env_rep::exec_hr_length () { return tree (TMLEN, "3600000"); }
