
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
#include "scheme.hpp"
#include "merge_sort.hpp"

/******************************************************************************
* Main execution
******************************************************************************/

tree
edit_env_rep::exec_anim_static (tree t) {
  if (N(t) < 3) return tree (TMERROR, "bad anim-static");
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
  if (u == "" && env->anim_portion > 0.001) return u;
  else if (env->anim_portion < 0.999) return t;
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
morph_with (tree t0, tree t1, edit_env env) {
  if (!is_func (t0, WITH))
    return morph_with (tree (WITH, t0), t1, env);
  if (!is_func (t1, WITH))
    return morph_with (t0, tree (WITH, t1), env);
  tree r (WITH);
  for (int i0=0; i0+1<N(t0); i0+=2) {
    tree v0= t0[i0+1], v1;
    int i1;
    for (i1=0; i1+1<N(t1); i1+=2)
      if (t0[i0] == t1[i1]) {
        v1= t1[i1+1];
        break;
      }
    if (i1+1 >= N(t1)) v1= env->read (as_string (t0[i0]));
    r << t0[i0] << morph (v0, v1, env);
  }
  for (int i1=0; i1+1<N(t1); i1+=2) {
    bool found= false;
    for (int i0=0; i0+1<N(t0); i0+=2)
      if (t0[i0] == t1[i1]) { found= true; break; }
    if (found) continue;
    tree v0= env->read (as_string (t1[i1]));
    tree v1= t1[i1+1];
    r << t1[i1] << morph (v0, v1, env);
  }
  r << morph (t0[N(t0)-1], t1[N(t1)-1], env);
  return r;
}

static tree head (tree t) { return t (0, N(t) - 1); }
static tree prev (tree t) { return t[N(t)-2]; }
static tree tail (tree t) { return t[N(t)-1]; }

tree
morph_tformat (tree t0, tree t1, edit_env env) {
  tree r (TFORMAT);
  for (int i0=0; i0+1<N(t0); i0++)
    if (is_func (t0[i0], CWITH)) {
      int i1;
      tree v0= tail (t0[i0]), v1;
      for (i1=0; i1+1<N(t1); i1++)
        if (is_func (t1[i1], CWITH))
          if (head (t0[i0]) == head (t1[i1])) {
            v1= tail (t1[i1]);
            break;
          }
      if (i1+1 >= N(t1)) v1= env->read (as_string (prev (t0[i0])));
      tree w= copy (t0[i0]);
      w[N(w)-1]= morph (v0, v1, env);
      r << w;
    }
  for (int i1=0; i1+1<N(t1); i1++)
    if (is_func (t1[i1], CWITH)) {
      bool found= false;
      for (int i0=0; i0+1<N(t0); i0++)
        if (is_func (t0[i0], CWITH))
          if (head (t0[i0]) == head (t1[i1])) { found= true; break; }
      if (found) continue;
      tree v0= env->read (as_string (prev (t1[i1])));
      tree v1= tail (t1[i1]);
      tree w= copy (t1[i1]);
      w[N(w)-1]= morph (v0, v1, env);
      r << w;
    }
  r << morph (t0[N(t0)-1], t1[N(t1)-1], env);
  return r;
}

string
get_anim_id (tree t) {
  if (!is_func (t, WITH)) return "";
  for (int i=0; i+1<N(t); i++)
    if (t[i] == ANIM_ID)
      return as_string (t[i+1]);
  return get_anim_id (t[N(t)-1]);
}

tree
morph_graphics (tree t0, tree t1, edit_env env) {
  tree r (GRAPHICS, "");
  hashset<string> done;
  done->insert ("");
  int i0=0, i1=0;
  /*
  cout << "Morphing " << env->anim_start << ", " << env->anim_end
       << "; " << env->anim_portion << LF << HRULE << LF;
  for (int k=0; k<N(t0); k++)
    cout << k << ": " << t0[k] << LF;
  cout << HRULE << LF;
  for (int k=0; k<N(t1); k++)
    cout << k << ": " << t1[k] << LF;
  cout << HRULE << LF;
  */
  while (i0 < N(t0) && i1 < N(t1)) {
    string id0= get_anim_id (t0[i0]);
    string id1= get_anim_id (t1[i1]);
    if (done->contains (id0)) { i0++; continue; }
    if (done->contains (id1)) { i1++; continue; }
    if (id0 == id1 && id0 != "") {
      r << morph (t0[i0], t1[i1], env);
      done->insert (id0);
      done->insert (id1);
      i0++; i1++; continue;
    }
    if (id0 == "") {
      r << morph (t0[i0], "", env);
      i0++; continue;
    }
    if (id1 == "") {
      r << morph ("", t1[i1], env);
      i1++; continue;
    }
    int j0, j1;
    for (j0=i0+1; j0<N(t0); j0++)
      if (get_anim_id (t0[j0]) == id1) break;
    for (j1=i1+1; j1<N(t1); j1++)
      if (get_anim_id (t1[j1]) == id0) break;
    if (j1<N(t1) && (j0>=N(t0) || (j1-i1 <= j0-i0))) {
      r << morph (t0[i0], t1[j1], env);
      done->insert (id0);
      i0++; continue;
    }
    if (j0<N(t0) && (j1>=N(t1) || (j0-i0 <= j1-i1))) {
      r << morph (t0[j0], t1[i1], env);
      done->insert (id1);
      i1++; continue;
    }
    r << morph (t0[i0], "", env);
    r << morph ("", t1[i1], env);
    i0++; i1++;
  }
  for (; i0 < N(t0); i0++)
    if (!done->contains (get_anim_id (t0[i0])))
      r << morph (t0[i0], "", env);
  for (; i1 < N(t1); i1++)
    if (!done->contains (get_anim_id (t1[i1])))
      r << morph ("", t1[i1], env);
  return r;
}

bool
morphable_arity (tree_label l) {
  return
    (l >= LINE && l <= CSMOOTH) ||
    l == CONCAT || l == DOCUMENT;
}

tree
interpolate (tree t, int i, double x) {
  if (N(t) == 1 || i >= N(t)-1) return t[i];
  if (!is_func (t[i], _POINT) || !is_func (t[i+1], _POINT)) return "";
  if (N(t[i]) != N(t[i+1])) return "";
  tree r (_POINT, N(t[i]));
  for (int j=0; j<N(t[i]); j++) {
    double v= as_double (t[i  ][j]);
    double w= as_double (t[i+1][j]);
    r[j]= as_tree ((1-x) * v + x * w);
  }
  return r;
}

tree
complete_sub (tree t0, tree t1) {
  int n0= N(t0), n1= N(t1);
  if (n0 == n1) return t0;
  if (n0 == 0 ) return t1;
  
  int l=0, r=0;
  while (l < n0 && t0[l] == t1[l]) l++;
  if (l > 0) l--;
  while (r < n0-l && t0[n0-1-r] == t1[n1-1-r]) r++;
  if (r > 0) r--;
  if (l > 0 || r > 0) {
    tree c= complete_sub (t0 (l, n0-r), t1 (l, n1-r));
    return t0 (0, l) * c * t0 (n0-r, n0);
  }

  int h= n0>>1, ha= max (h, 1), hb= min (h, n0-2);
  for (int h0= ha; h0 <= hb; h0++)
    for (int h1=h0; h1<=h0+n1-n0; h1++)
      if (t0[h0] == t1[h1]) {
        tree r0= complete_sub (t0 (0 , h0+1), t1 (0 , h1+1));
        tree r1= complete_sub (t0 (h0, n0  ), t1 (h1, n1  ));
        return r0 (0, N(r0)-1) * r1;
      }
  
  tree r0 (L(t0), n1);
  int i0=-1, nr=0, tot= (n0==1? n1: (n1+n0-3)/(n0-1));
  for (int i1=0; i1<n1; i1++) {
    int next= (i1 * (n0 - 1)) / (n1 - 1);
    if (next != i0) {
      i0= next;
      nr= 0;
      r0[i1]= t0[i0];
    }
    else {
      nr++;
      double x= ((double) nr) / ((double) tot);
      r0[i1]= interpolate (t0, i0, x);
    }
  }
  return r0;
}

tree
complete (tree t0, tree t1) {
  tree_label l= L(t0);
  if (l == CLINE || l == CARC || l == CSPLINE ||
      l == CBEZIER || l == CSMOOTH) {
    tree r= complete_sub (t0 * t0 (0, 1), t1 * t1 (0, 1));
    return r (0, N(r)-1);
  }
  return complete_sub (t0, t1);
}

tree
morph_variable_arity (tree t0, tree t1, edit_env env) {
  if (env->anim_portion < 0.001) return t0;
  if (env->anim_portion > 0.999) return t1;
  if (N(t0) < N(t1)) return morph (complete (t0, t1), t1, env);
  if (N(t1) < N(t0)) return morph (t0, complete (t1, t0), env);
  return morph (t0, t1, env);
}

tree
morph (tree t0, tree t1, edit_env env) {
  //cout << "Morph " << t0 << ", " << t1 << ", " << env->anim_portion << "\n";
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
  else if (is_func (t0, WITH) || is_func (t1, WITH))
    return morph_with (t0, t1, env);
  else if (is_func (t0, TFORMAT) && is_func (t1, TFORMAT))
    return morph_tformat (t0, t1, env);
  else if (is_func (t0, VALUE) && t1 == t0)
    return t0;
  else if (is_func (t0, VALUE, 1) && is_atomic (t0[0]))
    return morph (env->read (t0[0]->label), t1, env);
  else if (is_func (t1, VALUE, 1) && is_atomic (t1[0]))
    return morph (t0, env->read (t1[0]->label), env);
  else if (is_atomic (t0) || is_atomic (t1))
    return morph_trivial (t0, t1, env);
  else if (is_func (t0, GRAPHICS) && is_func (t1, GRAPHICS))
    return morph_graphics (t0, t1, env);
  else if (L(t0) == L(t1) && N(t0) == N(t1)) {
    int i, n= N(t0);
    tree tt (t0, n);
    for (i=0; i<n; i++)
      tt[i]= morph (t0[i], t1[i], env);
    return tt;
  }
  else if (L(t0) == L(t1) && morphable_arity (L(t0)))
    return morph_variable_arity (t0, t1, env);
  else if (env->is_anylen (t0) && env->is_anylen (t1))
    return morph_length (t0, t1, env);
  else
    return morph_trivial (t0, t1, env);
}

/******************************************************************************
* Adding animated object identifiers
******************************************************************************/

tree insert_anim_ids (tree t);

array<tree>
insert_anim_ids (array<tree> a) {
  if (N(a) == 0) return a;
  bool same= true;
  for (int i=1; i<N(a); i++) {
    same= same && (is_atomic (a[0]) == is_atomic (a[i]));
    if (same && is_compound (a[0]))
      same= same && L(a[0]) == L(a[i]) && N(a[0]) == N(a[i]);
  }
  if (!same) {
    array<tree> b= copy (a);
    for (int i=0; i<N(a); i++)
      b[i]= insert_anim_ids (a[i]);
    return b;
  }
  else if (is_atomic (a[0])) return a;
  else {
    int n= N(a[0]);
    array<tree> r (N(a));
    for (int i=0; i<N(a); i++)
      r[i]= tree (a[i], n);
    for (int c=0; c<n; c++) {
      array<tree> b (N(a));
      for (int i=0; i<N(a); i++)
        b[i]= a[i][c];
      if (is_func (a[0], GRAPHICS)) {
        bool ins= true;
        for (int i=0; i<N(b); i++)
          ins= ins && (is_compound (b[i]) && get_anim_id (b[i]) == "");
        if (ins) {
          string id= as_string (call ("create-unique-id"));
          for (int i=0; i<N(b); i++) {
            if (!is_func (b[i], WITH)) b[i]= tree (WITH, b[i]);
            b[i]= tree (WITH, ANIM_ID, id) * b[i];
          }
        }
      }
      array<tree> rew= insert_anim_ids (b);
      for (int i=0; i<N(a); i++)
        r[i][c]= rew[i];
    }
    return r;
  }
}

tree
insert_anim_ids (tree t) {
  if (is_atomic (t)) return t;
  else if (is_func (t, MORPH))
    return tree (L(t), insert_anim_ids (A(t)));
  else {
    int i, n=N(t);
    tree r (t, n);
    for (i=0; i<n; i++) {
      r[i]= insert_anim_ids (t[i]);
      if (is_func (t, GRAPHICS) && is_compound (t[i])) {
        string id= get_anim_id (t[i]);
        if (id == "") {
          id= as_string (call ("create-unique-id"));
          if (!is_func (r[i], WITH)) r[i]= tree (WITH, r[i]);
          r[i]= tree (WITH, ANIM_ID, id) * r[i];
        }
      }
    }
    return r;
  }
}

/******************************************************************************
* Checking out one frame of an animation for editing
******************************************************************************/

double
round_portion (double portion) {
  return 0.001 * floor (1000.0 * portion + 0.5);
}

tree
edit_env_rep::checkout_animation (tree t) {
  if (N(t) < 4) return t;
  int tot= max (as_length (exec (t[1])), 1);
  int cur= max (as_length (exec (t[3])), 0);
  double old_start  = anim_start;
  double old_end    = anim_end;
  double old_portion= anim_portion;
  anim_start  = 0.0;
  anim_end    = 0.001 * tot;
  anim_portion= round_portion ((1.0 * cur) / (1.0 * tot));
  tree a= insert_anim_ids (t[0]);
  tree frame= animate (a);
  anim_start  = old_start;
  anim_end    = old_end;
  anim_portion= old_portion;
  return compound ("anim-edit", a, copy (frame), t[1], t[2], t[3]);
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
        if (fabs (t - x) < 1.0e-6) { b << ins; done= true; }
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
  int cur= max (as_length (exec (t[4])), 0);
  double portion= round_portion ((1.0 * cur) / (1.0 * tot));
  a[0]= insert_frame (a[0], insert_anim_ids (t[1]), portion);
  return a;
}

/******************************************************************************
* Obtaining the control times of an animation
******************************************************************************/

static void
get_control_times (tree t, array<double>& ts) {
  if (is_atomic (t)) return;
  int i=0, n=N(t);
  if (is_func (t, MORPH))
    for (i=0; i<n; i++)
      if (is_tuple (t[i]) && N(t[i]) >= 2 && is_double (t[i][0]))
        ts << as_double (t[i][0]);
  for (i=0; i<n; i++)
    get_control_times (t[i], ts);
}

array<double>
get_control_times (tree t) {
  array<double> a;
  get_control_times (t, a);
  merge_sort (a);
  array<double> r;
  for (int i=0; i<N(a); i++)
    if (i == 0 || a[i] != a[i-1])
      r << a[i];
  return r;
}

/******************************************************************************
* Morph expansion
******************************************************************************/

static string
as_nice_string (double x) {
  double x2= round (1000.0 * x) / 1000.0;
  if (fabs (x2 - x) < 1.0e-6) return as_string (x2);
  return as_string (x);
}

tree
edit_env_rep::expand_morph (tree t) {
  if (is_func (t, ANIM_STATIC, 4) ||
      is_func (t, ANIM_DYNAMIC, 4) ||
      is_compound (t, "anim-edit", 5)) {
    int tot= max (as_length (exec (t[N(t)-3])), 1);
    double old_start  = anim_start;
    double old_end    = anim_end;
    double old_portion= anim_portion;
    anim_start  = 0.0;
    anim_end    = 0.001 * tot;
    anim_portion= 0.0;
    tree r (t, N(t));
    r[0]= expand_morph (t[0]);
    for (int i=1; i<N(t); i++) r[i]= t[i];
    anim_start  = old_start;
    anim_end    = old_end;
    anim_portion= old_portion;
    return r;
  }
  else {
    array<double> a= get_control_times (t);
    if (N(a) == 0) return t;
    tree r (MORPH);
    for (int i=0; i<N(a); i++) {
      string s= as_nice_string (a[i]);
      double old_portion= anim_portion;
      anim_portion= a[i];
      tree frame= animate (a);
      anim_portion= old_portion;
      r << tuple (s, frame);
    }
    return r;
  }
}
