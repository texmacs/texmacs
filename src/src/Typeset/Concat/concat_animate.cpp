
/******************************************************************************
* MODULE     : concat_animate.cpp
* DESCRIPTION: "Typesetting" animations
* COPYRIGHT  : (C) 2005  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
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
  print (STD_ITEM, anim_compose_box (ip, bs));
}

void
concater_rep::typeset_anim_repeat (tree t, path ip) {
  box b= typeset_as_concat (env, t[0], descend (ip, 0));
  print (STD_ITEM, anim_repeat_box (ip, b));
}

void
concater_rep::typeset_anim_constant (tree t, path ip) {
  int l= env->as_length (env->exec (t[0]));
  box b= typeset_as_concat (env, t[1], descend (ip, 1));
  print (STD_ITEM, anim_constant_box (ip, b, l));
}

void
concater_rep::typeset_anim_effect (tree t, path ip) {
  (void) t;
  print (STD_ITEM, empty_box (ip));
}

/******************************************************************************
* Sounds
******************************************************************************/

void
concater_rep::typeset_sound (tree t, path ip) {
  tree sound_t= env->exec (t[0]);
  url sound= url_none ();
  if (is_atomic (sound_t)) {
    url sound_u= sound_t->label;
    sound= resolve (relative (env->base_file_name, sound_u));
  }
  if (!is_none (sound)) {
    int sz= script (env->fn_size, env->index_level);
    font gfn (tex_font (env->dis, "cmr", sz, (int) (env->magn*env->dpi)));
    print (STD_ITEM, sound_box (ip, sound, gfn->yx));
    flag ("sound", ip, env->dis->brown);
  }
  else typeset_dynamic (tree (ERROR, "bad sound", t), ip);
}

/******************************************************************************
* Videos
******************************************************************************/

void
concater_rep::typeset_video (tree t, path ip) {
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
    print (STD_ITEM, video_box (ip, video, w, h, len, rep));
  }
  else typeset_dynamic (tree (ERROR, "bad video", t), ip);
}
