
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
#include "blackbox.hpp"
#include "analyze.hpp"

extern tree the_et;

/******************************************************************************
* Basic constructs for animations
******************************************************************************/

bool
has_player (path ip) {
  path p= reverse (ip);
  if (!has_subtree (the_et, p)) return false;
  tree t= subtree (the_et, p);
  blackbox bb;
  return t->obs->get_contents (ADDENDUM_PLAYER, bb);
}

player
get_player (path ip) {
  path p= reverse (ip);
  if (has_subtree (the_et, p)) {
    tree t= subtree (the_et, p);
    blackbox bb;
    bool ok= t->obs->get_contents (ADDENDUM_PLAYER, bb);
    if (ok) return open_box<player> (bb);
  }
  return player ();
}

path
search_animation_ip (path ip) {
  if (is_nil (ip)) return ip;
  path p= reverse (ip);
  if (has_subtree (the_et, p)) {
    tree t= subtree (the_et, p);
    blackbox bb;
    bool ok= t->obs->get_contents (ADDENDUM_PLAYER, bb);
    if (ok) return ip;
  }
  return search_animation_ip (ip->next);
}

path
search_longest_ip (path ip) {
  path p= reverse (ip);
  if (has_subtree (the_et, p)) return ip;
  return search_longest_ip (ip->next);
}

path
undecorate (path ip) {
  if (!is_nil (ip) && ip->item < 0) {
    if (ip->item == DECORATION && !is_nil (ip->next)) return ip->next->next;
    else return ip->next;
  }
  else return ip;
}

path
edit_env_rep::get_animation_ip (path ip) {
  ip= undecorate (ip);
  path aip= search_animation_ip (ip);
  if (!is_nil (aip)) return aip;
  aip= search_longest_ip (ip);
  if (is_nil (aip)) return aip;
  tree t= subtree (the_et, reverse (aip));
  blackbox bb= close_box<player> (player ());
  (void) tree_addendum_new (t, ADDENDUM_PLAYER, bb, false);
  return aip;
}

/******************************************************************************
* Basic constructs for animations
******************************************************************************/

void
concater_rep::typeset_anim_compose (tree t, path ip) {
  player pl= get_player (env->get_animation_ip (ip));
  int i, n= N(t);
  array<box> bs(n);
  for (i=0; i<n; i++) bs[i]= typeset_as_concat (env, t[i], descend (ip, i));
  print (anim_compose_box (ip, bs, pl));
}

void
concater_rep::typeset_anim_repeat (tree t, path ip) {
  if (N(t) != 1) { typeset_error (t, ip); return; }
  player pl= get_player (env->get_animation_ip (ip));
  box b= typeset_as_concat (env, t[0], descend (ip, 0));
  print (anim_repeat_box (ip, b, pl));
}

void
concater_rep::typeset_anim_constant (tree t, path ip) {
  if (N(t) != 2) { typeset_error (t, ip); return; }
  player pl= get_player (env->get_animation_ip (ip));
  box b= typeset_as_concat (env, t[0], descend (ip, 0));
  int l= env->as_length (env->exec (t[1]));
  print (anim_constant_box (ip, b, pl, l));
}

player
accelerate (player pl, tree kind) {
  if (kind == "reverse") return reverse_player (pl);
  if (kind == "fade-in") return fade_in_player (pl);
  if (kind == "fade-out") return fade_out_player (pl);
  if (kind == "faded") return faded_player (pl);
  if (kind == "bump") return bump_player (pl);
  if (is_atomic (kind) && starts (kind->label, "reverse-"))
    return reverse_player (accelerate (pl, kind->label (8, N(kind->label))));
  if (is_tuple (kind, "fixed", 1) && is_atomic (kind[1]))
    return fixed_player (pl, as_double (kind[1]));
  return pl;
}

void
concater_rep::typeset_anim_accelerate (tree t, path ip) {
  if (N(t) != 2) { typeset_error (t, ip); return; }
  path uip= undecorate (ip);
  while (!has_subtree (the_et, reverse (uip))) uip= uip->next;
  player apl= get_player (uip);
  if (!is_nil (uip) && !has_player (uip)) {
    path aip= search_animation_ip (uip);
    player pl = is_nil (aip)? player (): get_player (aip);
    apl= accelerate (pl, t[1]);
    tree st= subtree (the_et, reverse (uip));
    blackbox bb= close_box<player> (apl);
    (void) tree_addendum_new (st, ADDENDUM_PLAYER, bb, false);
  }
  box b= typeset_as_concat (env, t[0], descend (ip, 0));
  apl->set_duration (b->anim_duration ());
  array<box> bs;
  bs << b;
  print (composite_box (ip, bs));
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
  player pl= get_player (env->get_animation_ip (ip));
  box  b  = typeset_as_concat (env, t[0], descend (ip, 0));
  int  len= env->as_length (env->exec (t[1]));
  tree t1 = env->exec (t[2]);
  tree t2 = env->exec (t[3]);
  SI x1= b->x1, y1= b->y1, x2= b->x1, y2= b->y1;
  if (is_tuple (t1) && N(t1)==2) effect_point (env, b, t1[0], t1[1], x1, y1);
  if (is_tuple (t2) && N(t2)==2) effect_point (env, b, t2[0], t2[1], x2, y2);
  print (anim_translate_box (ip, b, pl, len, x1, y1, x2, y2));
}

void
concater_rep::typeset_anim_progressive (tree t, path ip) {
  if (N(t) != 4) { typeset_error (t, ip); return; }
  player pl= get_player (env->get_animation_ip (ip));
  box  b  = typeset_as_concat (env, t[0], descend (ip, 0));
  int  len= env->as_length (env->exec (t[1]));
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
  print (anim_progressive_box (ip, b, pl, len, r1, r2));
}

/******************************************************************************
* Sounds
******************************************************************************/

void
concater_rep::typeset_sound (tree t, path ip) {
  if (N(t) != 1) { typeset_error (t, ip); return; }
  player pl= get_player (env->get_animation_ip (ip));
  tree sound_t= env->exec (t[0]);
  url sound= url_none ();
  if (is_atomic (sound_t)) {
    url sound_u= sound_t->label;
    sound= resolve (relative (env->base_file_name, sound_u));
  }
  if (!is_none (sound)) {
    int sz= script (env->fn_size, env->index_level);
    font gfn (tex_font ("cmr", sz, (int) (env->magn*env->dpi)));
    print (sound_box (ip, pl, sound, gfn->yx));
    flag ("sound", ip, brown);
  }
  else typeset_dynamic (tree (_ERROR, "bad sound", t[0]), ip);
}

/******************************************************************************
* Videos
******************************************************************************/

void
concater_rep::typeset_video (tree t, path ip) {
  if (N(t) != 5) { typeset_error (t, ip); return; }
  player pl= get_player (env->get_animation_ip (ip));
  tree video_t= env->exec (t[0]);
  url video= url_none ();
  if (is_atomic (video_t)) {
    url video_u= video_t->label;
    video= resolve (relative (env->base_file_name, video_u));
  }
  if (!is_none (video)) {
    SI   w  = env->as_length (env->exec (t[1]));
    SI   h  = env->as_length (env->exec (t[2]));
    int  len= env->as_length (env->exec (t[3]));
    bool rep= env->exec (t[4]) != "false";
    print (video_box (ip, pl, video, w, h, env->alpha, len, rep, env->pixel));
  }
  else typeset_dynamic (tree (_ERROR, "bad video", t[0]), ip);
}

/******************************************************************************
* Extra routines for controlling the animation players
******************************************************************************/

void
players_set_elapsed (tree t, double el) {
  blackbox bb;
  bool ok= t->obs->get_contents (ADDENDUM_PLAYER, bb);
  if (ok) {
    player pl= open_box<player> (bb);
    pl->set_elapsed (el);
  }
  if (is_compound (t)) {
    int i, n= N(t);
    for (i=0; i<n; i++)
      players_set_elapsed (t[i], el);
  }
}

void
players_set_speed (tree t, double sp) {
  blackbox bb;
  bool ok= t->obs->get_contents (ADDENDUM_PLAYER, bb);
  if (ok) {
    player pl= open_box<player> (bb);
    pl->set_speed (sp);
  }
  if (is_compound (t)) {
    int i, n= N(t);
    for (i=0; i<n; i++)
      players_set_elapsed (t[i], sp);
  }
}
