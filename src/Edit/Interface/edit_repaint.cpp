
/******************************************************************************
* MODULE     : edit_repaint.cpp
* DESCRIPTION: repaint invalid rectangles
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "Interface/edit_interface.hpp"
#include "message.hpp"
#include "gui.hpp" // for gui_interrupted

extern int nr_painted;
extern void clear_pattern_rectangles (renderer ren, rectangle m, rectangles l);
extern bool animated_flag;

/******************************************************************************
* repainting the window
******************************************************************************/

void
edit_interface_rep::draw_background (renderer ren,
                                     SI x1, SI y1, SI x2, SI y2) {
  tree bg= get_init_value (BG_COLOR);
  ren->set_background (bg);
  if (get_init_value (PAGE_MEDIUM) == "paper")
    eb->clear (ren, x1, y1, x2, y2);
  else {
    rectangle m (eb->x1, eb->y1, eb->x2, eb->y2);
    rectangle r (x1, y1, x2, y2);
    rectangle tm= translate (m, ren->ox, ren->oy);
    rectangle tr= translate (r, ren->ox, ren->oy);
    clear_pattern_rectangles (ren, tm, tr);
  }
}

void
edit_interface_rep::draw_text (renderer ren, rectangles& l) {
  nr_painted=0;
  bool tp_found= false;
  tree bg= get_init_value (BG_COLOR);
  ren->set_background (bg);
  animated_flag= (texmacs_time () >= anim_next);
  if (animated_flag) anim_next= 1.0e12;
  eb->redraw (ren, eb->find_box_path (tp, tp_found), l);
  if (animated_flag) {
    double t= max (((double) texmacs_time ()) + 25.0, eb->anim_next ());
    anim_next= min (anim_next, t);
  }
}

void
edit_interface_rep::draw_env (renderer ren) {
  if (!full_screen) {
    if (!is_nil (env_rects)) {
      ren->set_pencil (pencil (rgb_color (0, 85, 85, 24), ren->pixel));
      ren->draw_rectangles (env_rects);
    }
    if (!is_nil (foc_rects)) {
      ren->set_pencil (pencil (rgb_color (0, 255, 255), ren->pixel));
      ren->draw_rectangles (foc_rects);
    }
    if (!is_nil (sem_rects)) {
      if (sem_correct)
        ren->set_pencil (pencil (rgb_color (112, 208, 112), ren->pixel));
      else
        ren->set_pencil (pencil (rgb_color (208, 144, 80), ren->pixel));
      ren->draw_rectangles (sem_rects);
    }
  }
}

void
edit_interface_rep::draw_cursor (renderer ren) {
  if (get_preference ("draw cursor") == "on" &&
      !temp_invalid_cursor && (got_focus || full_screen)) {
    cursor cu= get_cursor();
    if (!inside_active_graphics ()) {
      cu->y1 -= 2*pixel; cu->y2 += 2*pixel;
      SI x1= cu->ox + ((SI) (cu->y1 * cu->slope)), y1= cu->oy + cu->y1;
      SI x2= cu->ox + ((SI) (cu->y2 * cu->slope)), y2= cu->oy + cu->y2;
      string mode= get_env_string (MODE);
      string family, series;
      color cuc= red;
      if (!cu->valid) cuc= green;
      else if (mode == "math") cuc= rgb_color (192, 0, 255);
      ren->set_pencil (pencil (cuc, pixel));
      if ((mode == "text") || (mode == "src")) {
        family= get_env_string (FONT_FAMILY);
        series= get_env_string (FONT_SERIES);
      }
      else if (mode == "math") {
        family= get_env_string (MATH_FONT_FAMILY);
        series= get_env_string (MATH_FONT_SERIES);
      }
      else if (mode == "prog") {
        family= get_env_string (PROG_FONT_FAMILY);
        series= get_env_string (PROG_FONT_SERIES);
      }
      SI lserif= (series=="bold"? 2*pixel: pixel), rserif= pixel;
      if (family == "ss") lserif= rserif= 0;
      ren->line (x1-lserif, y1, x1+rserif, y1);
      if (y1<=y2-pixel) {
        ren->line (x1, y1, x2, y2-pixel);
        if (series == "bold") ren->line (x1-pixel, y1, x2-pixel, y2-pixel);
        ren->line (x2-lserif, y2-pixel, x2+rserif, y2-pixel);
      }
    }
  }
}

void
edit_interface_rep::draw_surround (renderer ren, rectangle r) {          
  ren->set_background (tm_background);
  string medium= get_init_string (PAGE_MEDIUM);
  if (medium == "automatic") return;
  if (medium == "beamer" && full_screen) return;
  ren->clear (r->x1, r->y1, max (r->x1, eb->x1), r->y2);
  ren->clear (min (r->x2, eb->x2), r->y1, r->x2, r->y2);
  if (medium == "papyrus") return;
  ren->clear (r->x1, r->y1, r->x2, max (r->y1, eb->y1));
  ren->clear (r->x1, min (r->y2, eb->y2), r->x2, r->y2);
}

void
edit_interface_rep::draw_context (renderer ren, rectangle r) {
  draw_surround (ren, r);
}

void
edit_interface_rep::draw_selection (renderer ren, rectangle r) {
  rectangles visible (thicken (r, 2 * ren->pixel, 2 * ren->pixel));
  if (!is_nil (locus_rects)) {
    ren->set_pencil (pencil (rgb_color (32, 160, 96), ren->pixel));
    ren->draw_rectangles (locus_rects);
  }
  for (int i=0; i<N(alt_selection_rects); i++) {
    ren->set_pencil (pencil (rgb_color (240, 192, 0), ren->pixel));
#ifdef QTTEXMACS
    ren->draw_selection (alt_selection_rects[i] & visible);
#else
    ren->draw_rectangles (alt_selection_rects[i] & visible);
#endif
  }
  if (!is_nil (selection_rects)) {
    color col= (table_selection? rgb_color (192, 0, 255): red);
    ren->set_pencil (pencil (col, ren->pixel));
#ifdef QTTEXMACS
    ren->draw_selection (selection_rects & visible);
#else
    ren->draw_rectangles (selection_rects & visible);
#endif
  }
}

void
edit_interface_rep::draw_graphics (renderer ren) {
  if (got_focus || full_screen) {
    cursor cu= get_cursor();
    if (over_graphics(cu->ox, cu->oy) && inside_active_graphics ()) {
      eval ("(graphics-reset-context 'graphics-cursor)");
      draw_graphical_object (ren);
      string tm_curs= as_string (eval ("graphics-texmacs-pointer"));
      if (tm_curs != "none") {
        if (tm_curs == "graphics-cross") {
          ren->set_pencil (pencil (red, pixel));
          ren->line (cu->ox, cu->oy-5*pixel, cu->ox, cu->oy+5*pixel);
          ren->line (cu->ox-5*pixel, cu->oy, cu->ox+5*pixel, cu->oy);
        }
        else if (tm_curs == "graphics-cross-arrows") {
          static int s= 6*pixel, a= 2*pixel;
          ren->set_pencil (pencil (red, pixel));
          ren->line (cu->ox, cu->oy-s, cu->ox, cu->oy+s);
          ren->line (cu->ox-s, cu->oy, cu->ox+s, cu->oy);
          ren->line (cu->ox, cu->oy-s,cu->ox-a, cu->oy-s+a);
          ren->line (cu->ox, cu->oy-s, cu->ox+a, cu->oy-s+a);
          ren->line (cu->ox, cu->oy+s, cu->ox-a, cu->oy+s-a);
          ren->line (cu->ox, cu->oy+s, cu->ox+a, cu->oy+s-a);
          ren->line (cu->ox-s, cu->oy, cu->ox-s+a, cu->oy+a);
          ren->line (cu->ox-s, cu->oy, cu->ox-s+a, cu->oy-a);
          ren->line (cu->ox+s, cu->oy, cu->ox+s-a, cu->oy+a);
          ren->line (cu->ox+s, cu->oy, cu->ox+s-a, cu->oy-a);
        }
      }
    }
    else eval ("(graphics-reset-context 'text-cursor)");
  }
}

void
edit_interface_rep::draw_keys (renderer ren) {
  if (kbd_show_keys && got_focus && N(kbd_shown_keys) > 0 &&
      !is_nil (keys_rects) &&
      vy2 - vy1 > 3 * (keys_rects->item->y2 - keys_rects->item->y1)) {
    string s;
    for (int i=0; i<N(kbd_shown_keys); i++) {
      if (i>0) s << " ";
      s << kbd_shown_keys[i];
    }
    tree rew= get_server () -> kbd_system_rewrite (s);
    if (!is_concat (rew)) rew= tree (CONCAT, rew);
    string ns;
    for (int i=0; i<N(rew); i++) {
      tree t= rew[i];
      while (is_compound (t, "render-key") || is_func (t, WITH))
        t= t[N(t)-1];
      if (is_atomic (t)) {
        if (N(ns) != 0) ns << "  ";
        ns << t->label;
      }
    }
    ren->set_background (rgb_color (240, 224, 208));
    rectangle r= keys_rects->item;
    ren->clear (r->x1, r->y1, r->x2, r->y2);
    font fn;
    if (use_macos_fonts ()) {
      tree t= tuple ("apple-lucida", "ss", "medium", "right");
      t << tree ("14") << tree ("600");
      fn= find_font (t);
    }
    else {
      tree t= tuple ("pagella", "rm", "medium", "right");
      t << tree ("14") << tree ("600");
      fn= find_font (t);
    }
    metric ex;
    fn->get_extents (ns, ex);
    SI dx= (r->x2 - r->x1 + ex->x1 - ex->x2) >> 1;
    SI dy= (r->y2 - r->y1) / 3;
    SI dp= 25 * pixel;
    if ((ex->x2 - ex->x1 + dp) > (r->x2 - r->x1))
      dx= r->x2 - r->x1 + ex->x1 - ex->x2 - dp;
    ren->set_pencil (pencil (rgb_color (0, 0, 64)));
    fn->draw (ren, ns, r->x1 + dx, r->y1 + dy);
  }
}

void
edit_interface_rep::draw_pre (renderer win, renderer ren, rectangle r) {
  // draw surroundings
  draw_background (ren, r->x1, r->y1, r->x2, r->y2);
  draw_surround (ren, r);

  // predraw cursor
  draw_cursor (ren);
  rectangles l= copy_always;
  while (!is_nil (l)) {
    rectangle lr (l->item);
    win->put_shadow (ren, lr->x1, lr->y1, lr->x2, lr->y2);
    l= l->next;
  }
}

void
edit_interface_rep::draw_post (renderer win, renderer ren, rectangle r) {
  win->set_zoom_factor (zoomf);
  ren->set_zoom_factor (zoomf);
  draw_context (ren, r);
  draw_env (ren);
  draw_selection (ren, r);
  draw_graphics (ren);
  draw_cursor (ren); // the text cursor must be drawn over the graphical object
  draw_keys (ren);
  ren->reset_zoom_factor ();
  win->reset_zoom_factor ();
}

void
edit_interface_rep::draw_with_shadow (renderer win, rectangle r) {
  rectangle sr= r * magf;
  win->new_shadow (shadow);
  win->get_shadow (shadow, sr->x1, sr->y1, sr->x2, sr->y2);
  renderer ren= shadow;

  rectangles l;
  win->set_zoom_factor (zoomf);
  ren->set_zoom_factor (zoomf);
  draw_pre (win, ren, r);
  draw_text (ren, l);
  ren->reset_zoom_factor ();
  win->reset_zoom_factor ();

  if (gui_interrupted ()) {
    ren->set_zoom_factor (zoomf);
    l= l & rectangles (translate (r, ren->ox, ren->oy));
    simplify (l);
    copy_always= translate (copy_always, ren->ox, ren->oy);
    while (!is_nil (copy_always)) {
      l= rectangles (copy_always->item, l);
      copy_always= copy_always->next;
    }
    ren->reset_zoom_factor ();

    draw_post (win, ren, r);
    while (!is_nil(l)) {
      SI x1= ((SI) (l->item->x1 * magf)) - ren->ox - PIXEL;
      SI y1= ((SI) (l->item->y1 * magf)) - ren->oy - PIXEL;
      SI x2= ((SI) (l->item->x2 * magf)) - ren->ox + PIXEL;
      SI y2= ((SI) (l->item->y2 * magf)) - ren->oy + PIXEL;
      ren->outer_round (x1, y1, x2, y2);
      win->put_shadow (ren, x1, y1, x2, y2);
      l= l->next;
    }
  }
}

void
edit_interface_rep::draw_with_stored (renderer win, rectangle r) {
  //cout << "Redraw " << (r*magf/PIXEL) << "\n";

  /* Verify whether the backing store is still valid */
  if (!is_nil (stored_rects)) {
    SI w1, h1, w2, h2;
    win    -> get_extents (w1, h1);
    stored -> get_extents (w2, h2);
    if (stored->ox != win->ox || stored->oy != win->oy ||
        w1 != w2 || h1 != h2) {
      // cout << "x"; cout.flush ();
      stored_rects= rectangles ();
    }
  }

  /* Either draw with backing store or regenerate */
  rectangle sr= r * magf;
  if (is_nil (rectangles (r) - stored_rects) && !is_nil (stored_rects)) {
    // cout << "*"; cout.flush ();
    win->new_shadow (shadow);
    win->get_shadow (shadow, sr->x1, sr->y1, sr->x2, sr->y2);
    shadow->put_shadow (stored, sr->x1, sr->y1, sr->x2, sr->y2);
    draw_post (win, shadow, r);
    win->put_shadow (shadow, sr->x1, sr->y1, sr->x2, sr->y2);
  }
  else {
    // cout << "."; cout.flush ();
    draw_with_shadow (win, r);
    if (!gui_interrupted ()) {
      if (inside_active_graphics ()) {
        shadow->new_shadow (stored);
        shadow->get_shadow (stored, sr->x1, sr->y1, sr->x2, sr->y2);
        //stored_rects= /*stored_rects |*/ rectangles (r);
        stored_rects= simplify (rectangles (r, stored_rects));
        //cout << "Stored: " << stored_rects << "\n";
        //cout << "M"; cout.flush ();
      }
      draw_post (win, shadow, r);
      win->put_shadow (shadow, sr->x1, sr->y1, sr->x2, sr->y2);
    }
    else draw_post (win, win, r);
  }
}

/******************************************************************************
* event handlers
******************************************************************************/

void
edit_interface_rep::handle_clear (renderer win, SI x1, SI y1, SI x2, SI y2) {
  x1= (SI) (x1 / magf); y1= (SI) (y1 / magf);
  x2= (SI) (x2 / magf); y2= (SI) (y2 / magf);
  win->set_zoom_factor (zoomf);
  draw_background (win, max (eb->x1, x1), max (eb->y1, y1),
                        min (eb->x2, x2), min (eb->y2, y2));
  draw_surround (win, rectangle (x1, y1, x2, y2));
  win->reset_zoom_factor ();
}

void
edit_interface_rep::handle_repaint (renderer win, SI x1, SI y1, SI x2, SI y2) {
  if (is_nil (eb)) apply_changes ();
  if (env_change != 0) {
    std_warning << "Invalid situation (" << env_change << ")"
                << " in edit_interface_rep::handle_repaint\n";
    return;
  }

  /*
  // In the past, we used the code below in order to hide the trace of
  // a moving cursor. This code is now incorrect, because the rectangle
  // (x1, y1)--(x2, y2) does not correspond to the repaint region clipping.
  // Nevertheless, the code seems no longer necessary. In case it would be,
  // it should be moved somewhere inside the internal repaint routines.
  SI extra= 3 * get_init_int (FONT_BASE_SIZE) * PIXEL * magf / 2;
  SI X1= (x1-extra) / magf, Y1= (y1-extra) / magf;
  SI X2= (x2+extra) / magf, Y2= (y2+extra) / magf;
  draw_with_stored (rectangle (X1, Y1, X2, Y2));
  */

  // cout << "Repainting\n";
  draw_with_stored (win, rectangle (x1, y1, x2, y2) /magf);
  if (last_change-last_update > 0)
    last_change = texmacs_time ();
  // cout << "Repainted\n";
}
