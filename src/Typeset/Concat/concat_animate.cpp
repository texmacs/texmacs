
/******************************************************************************
* MODULE     : concat_animate.cpp
* DESCRIPTION: "Typesetting" animations
* COPYRIGHT  : (C) 2005  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "concater.hpp"

/******************************************************************************
* Basic constructs for animations
******************************************************************************/

void
concater_rep::typeset_anim_compose (tree t, path ip) {
  int i, n= N(t);
  array<box> bs(n);
  for (i=0; i<n; i++) bs[i]= typeset_as_concat (env, t[i], descend (ip, i));
  print (anim_compose_box (ip, bs));
}

void
concater_rep::typeset_anim_repeat (tree t, path ip) {
  if (N(t) != 1) { typeset_error (t, ip); return; }
  box b= typeset_as_concat (env, t[0], descend (ip, 0));
  print (anim_repeat_box (ip, b));
}

void
concater_rep::typeset_anim_constant (tree t, path ip) {
  if (N(t) != 2) { typeset_error (t, ip); return; }
  box b= typeset_as_concat (env, t[0], descend (ip, 0));
  int l= env->as_length (env->exec (t[1]));
  print (anim_constant_box (ip, b, l));
}

static void
effect_point (edit_env env, box b, tree xt, tree yt, SI& x, SI& y) {
  if (is_double (xt)) x= as_int (b->x1 + as_double (xt) * b->w ());
  else x= env->as_length (xt);
  if (is_double (yt)) y= as_int (b->y1 + as_double (yt) * b->h ());
  else y= env->as_length (yt);
}

void
concater_rep::typeset_anim_translate (tree t, path ip) {
  if (N(t) != 4) { typeset_error (t, ip); return; }
  box b  = typeset_as_concat (env, t[0], descend (ip, 0));
  int  len= env->as_length (t[1]);
  tree t1 = env->exec (t[2]);
  tree t2 = env->exec (t[3]);
  SI x1= b->x1, y1= b->y1, x2= b->x1, y2= b->y1;
  if (is_tuple (t1) && N(t1)==2) effect_point (env, b, t1[0], t1[1], x1, y1);
  if (is_tuple (t2) && N(t2)==2) effect_point (env, b, t2[0], t2[1], x2, y2);
  print (anim_translate_box (ip, b, len, x1, y1, x2, y2));
}

void
concater_rep::typeset_anim_progressive (tree t, path ip) {
  if (N(t) != 4) { typeset_error (t, ip); return; }
  box b  = typeset_as_concat (env, t[0], descend (ip, 0));
  int  len= env->as_length (t[1]);
  tree t1 = env->exec (t[2]);
  tree t2 = env->exec (t[3]);
  rectangle r1 (b->x1, b->y1, b->x2, b->y2);
  rectangle r2 (b->x1, b->y1, b->x2, b->y2);
  if (is_tuple (t1) && N(t1)==4) {
    effect_point (env, b, t1[0], t1[1], r1->x1, r1->y1);
    effect_point (env, b, t1[2], t1[3], r1->x2, r1->y2);
  }
  if (is_tuple (t2) && N(t2)==4) {
    effect_point (env, b, t2[0], t2[1], r2->x1, r2->y1);
    effect_point (env, b, t2[2], t2[3], r2->x2, r2->y2);
  }
  print (anim_progressive_box (ip, b, len, r1, r2));
}

/******************************************************************************
* Sounds
******************************************************************************/

void
concater_rep::typeset_sound (tree t, path ip) {
  if (N(t) != 1) { typeset_error (t, ip); return; }
  tree sound_t= env->exec (t[0]);
  url sound= url_none ();
  if (is_atomic (sound_t)) {
    url sound_u= sound_t->label;
    sound= resolve (relative (env->base_file_name, sound_u));
  }
  if (!is_none (sound)) {
    int sz= script (env->fn_size, env->index_level);
    font gfn (tex_font ("cmr", sz, (int) (env->magn*env->dpi)));
    print (sound_box (ip, sound, gfn->yx));
    flag ("sound", ip, brown);
  }
  else typeset_dynamic (tree (ERROR, "bad sound", t[0]), ip);
}

/******************************************************************************
* Videos
******************************************************************************/

void
concater_rep::typeset_video (tree t, path ip) {
  if (N(t) != 5) { typeset_error (t, ip); return; }
  tree video_t= env->exec (t[0]);
  url video= url_none ();
  if (is_atomic (video_t)) {
    url video_u= video_t->label;
    video= resolve (relative (env->base_file_name, video_u));
  }
  if (!is_none (video)) {
    SI   w  = env->as_length (t[1]);
    SI   h  = env->as_length (t[2]);
    int  len= env->as_length (t[3]);
    bool rep= env->exec (t[4]) != "false";
    print (video_box (ip, video, w, h, len, rep));
  }
  else typeset_dynamic (tree (ERROR, "bad video", t[0]), ip);
}
