
/******************************************************************************
* MODULE     : env_animate.cpp
* DESCRIPTION: generation of animations
* COPYRIGHT  : (C) 2016  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "env.hpp"

/******************************************************************************
* Main execution
******************************************************************************/

tree
edit_env_rep::exec_anim_static (tree t) {
  if (N(t) < 3) return tree (ERROR, "bad anim-static");
  int tot= max (as_length (exec (t[1])), 1);
  int inc= max (as_length (exec (t[2])), 1);
  int cur= 0;
  double old_start  = anim_start;
  double old_end    = anim_end;
  double old_portion= anim_portion;
  anim_start  = 0.0;
  anim_end    = 0.001 * tot;
  tree r (ANIM_COMPOSE);
  while (true) {
    anim_portion= (1.0 * cur) / (1.0 * tot);
    int delta= min (tot - cur, inc);
    r << tree (ANIM_CONSTANT, animate (t[0]), as_string (delta) * "ms");
    if (cur >= tot) break;
    cur += delta;
  }
  anim_start  = old_start;
  anim_end    = old_end;
  anim_portion= old_portion;
  return r;
}

tree
edit_env_rep::exec_anim_dynamic (tree t) {
  return exec_anim_static (t);
}

tree
edit_env_rep::exec_morph (tree t) {
  return animate (t);
}

tree
edit_env_rep::exec_anim_time () {
  double anim_current= anim_start + anim_portion * (anim_end - anim_start);
  return as_string ((int) (1000.0 * anim_current)) * "ms";
}

tree
edit_env_rep::exec_anim_portion () {
  return as_string (anim_portion);
}

/******************************************************************************
* Expansion of animations
******************************************************************************/

tree morph (tree t, tree u, edit_env env);

tree
edit_env_rep::animate (tree t) {
  if (is_atomic (t)) return t;
  else if (is_func (t, MORPH)) {
    int i0= -1, i1= -1;
    double tt= anim_portion;
    double t0= -1000.0, t1= 1000.0;
    tree c0= tree (UNINIT), c1= tree (UNINIT);
    for (int i=0; i<N(t); i++)
      if (is_tuple (t[i]) && N(t[i]) >= 2 && is_double (t[i][0])) {
        double ti= as_double (t[i][0]);
        if (ti-1.0e-6 <= tt && ti > t0) { i0= i; t0= ti; c0= t[i][1]; }
        if (ti+1.0e-6 >= tt && ti < t1) { i1= i; t1= ti; c1= t[i][1]; }
      }
      else if (!is_tuple (t[i]) && i0 == -1) { i0= i; t0= 0.0; c0= t[i]; }
      else if (!is_tuple (t[i]) && i1 == -1) { i1= i; t1= 1.0; c1= t[i]; }
    if (i0 == -1 && i1 == -1) return "";
    if (i0 != -1 && i1 == -1) return c0;
    if (i0 == -1 && i1 != -1) return c1;
    double old_start  = anim_start;
    double old_end    = anim_end;
    double old_portion= anim_portion;
    anim_start  = old_start + t0 * (old_end - old_start);
    anim_end    = old_start + t1 * (old_end - old_start);
    anim_portion= old_start + tt * (old_end - old_start);
    double dt= anim_portion - anim_start;
    double d1= max (anim_end - anim_start, 1.0e-6);
    anim_portion= dt / d1;
    tree r= morph (c0, c1, edit_env (this));
    anim_start  = old_start;
    anim_end    = old_end;
    anim_portion= old_portion;
    return r;
  }
  else {
    int i, n= N(t);
    tree r (t, n);
    for (i=0; i<n; i++)
      r[i]= animate (t[i]);
    return r;
  }
}

/******************************************************************************
* Morphing
******************************************************************************/

tree
morph_trivial (tree t, tree u, edit_env env) {
  if (env->anim_portion < 0.5) return t;
  else return u;
}

string
morph_number (string s0, string s1, double t) {
  double x0= as_double (s0);
  double x1= as_double (s1);
  double xt= x0 + t * (x1 - x0);
  return as_string (xt);
}

tree
morph_length (tree t0, tree t1, edit_env env) {
  double t = env->anim_portion;
  if (fabs (t - 0.0) < 0.000001) return t0;
  if (fabs (t - 1.0) < 0.000001) return t1;
  if (t0 == t1) return t0;
  if (is_atomic (t0) && is_atomic (t1)) {
    SI x0, x1;
    string u0, u1;
    env->get_length_unit (t0->label, x0, u0);
    env->get_length_unit (t1->label, x1, u1);
    if (u0 == u1) {
      double l0= as_double (t0->label (0, N(t0->label) - N(u0)));
      double l1= as_double (t1->label (0, N(t1->label) - N(u1)));
      double lt= (1.0-t) * l0 + t * l1;
      return as_string (lt) * u0;
    }
  }
  tree a0= as_string (1.0 - t);
  tree a1= as_string (t);
  tree expr= tree (PLUS, tree (TIMES, a0, t0), tree (TIMES, a1, t1));
  return env->exec (expr);
}

string
morph_color (string s0, string s1, double t) {
  color c0= named_color (s0);
  int r0, g0, b0, a0;
  get_rgb_color (c0, r0, g0, b0, a0);
  color c1= named_color (s1);
  int r1, g1, b1, a1;
  get_rgb_color (c1, r1, g1, b1, a1);
  int rt= (int) (1.0 * r0 + t * (r1 - r0));
  int gt= (int) (1.0 * g0 + t * (g1 - g0));
  int bt= (int) (1.0 * b0 + t * (b1 - b0));
  int at= (int) (1.0 * a0 + t * (a1 - a0));
  color ct= rgb_color (rt, gt, bt, at);
  return get_hex_color (ct);
}

tree
morph (tree t0, tree t1, edit_env env) {
  if (is_atomic (t0) && is_atomic (t1)) {
    double t = env->anim_portion;
    string s0= t0->label;
    string s1= t1->label;
    if (is_double (s0) && is_double (s1))
      return morph_number (s0, s1, t);
    else if (starts (s0, "#") && starts (s1, "#"))
      return morph_color (s0, s1, t);
    else if (env->is_length (s0) && env->is_length (s1))
      return morph_length (t0, t1, env);
    else if (is_color_name (s0) && is_color_name (s1))
      return morph_color (s0, s1, t);
    else return morph_trivial (t0, t1, env);
  }
  else if (is_atomic (t0) || is_atomic (t1))
    return morph_trivial (t0, t1, env);
  else if (L(t0) == L(t1) && N(t0) == N(t1)) {
    int i, n= N(t0);
    tree tt (t0, n);
    for (i=0; i<n; i++)
      tt[i]= morph (t0[i], t1[i], env);
    return tt;
  }
  else if (env->is_anylen (t0) && env->is_anylen (t1))
    return morph_length (t0, t1, env);
  else
    return morph_trivial (t0, t1, env);
}

/******************************************************************************
* Checking out one frame of an animation for editing
******************************************************************************/

tree
edit_env_rep::checkout_animation (tree t) {
  if (N(t) < 4) return t;
  int tot= max (as_length (exec (t[1])), 1);
  int cur= max (as_length (exec (t[3])), 1);
  double old_start  = anim_start;
  double old_end    = anim_end;
  double old_portion= anim_portion;
  anim_start  = 0.0;
  anim_end    = 0.001 * tot;
  anim_portion= (1.0 * cur) / (1.0 * tot);
  tree frame= animate (t[0]);
  anim_start  = old_start;
  anim_end    = old_end;
  anim_portion= old_portion;
  return compound ("anim-edit", t[0], frame, t[1], t[2], t[3]);
}

/******************************************************************************
* Commit one frame of an animation after editing
******************************************************************************/

tree
insert_frame (tree a, tree f, double t) {
  if (is_func (a, MORPH)) {
    bool done= false;
    tree b (MORPH);
    tree ins (TUPLE, as_string (t), f);
    for (int i=0; i<N(a); i++) {
      if (is_tuple (a[i]) && N(a[i]) >= 2 && is_double (a[i][0]) && !done) {
        double x= as_double (a[i][0]);
        if (ins[0] == a[i][0]) b << ins;
        else if (t < x) { b << ins << a[i]; done= true; }
        else b << a[i];
      }
      else b << a[i];
    }
    if (!done) b << ins;
    return b;
  }
  else {
    tree r (MORPH, tree (TUPLE, "0", a),
                   tree (TUPLE, as_string (t), f));
    return r;
  }
}

tree
edit_env_rep::commit_animation (tree t) {
  if (N(t) < 5) return t;
  tree a= tree (ANIM_STATIC, t[0], t[2], t[3], t[4]);
  tree u= checkout_animation (a);
  if (u[1] == t[1]) return a;
  int tot= max (as_length (exec (t[2])), 1);
  int cur= max (as_length (exec (t[4])), 1);
  double portion= (1.0 * cur) / (1.0 * tot);
  a[0]= insert_frame (a[0], t[1], portion);
  return a;
}
