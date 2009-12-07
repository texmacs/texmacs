
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

extern int nr_painted;
extern void clear_pattern_rectangles (renderer ren, rectangles l);

/******************************************************************************
* repainting the window
******************************************************************************/

void
edit_interface_rep::draw_text (renderer ren, rectangles& l) {
  nr_painted=0;
  bool tp_found= false;
  tree bg= get_init_value (BG_COLOR);
  ren->set_background_pattern (bg);
  refresh_needed= do_animate;
  refresh_next  = next_animate;
  eb->redraw (ren, eb->find_box_path (tp, tp_found), l);
  do_animate  = refresh_needed;
  next_animate= refresh_next;
}

void
edit_interface_rep::draw_env (renderer ren) {
  if (!full_screen) {
    rectangles rs= env_rects;
    while (!is_nil (rs)) {
      ren->set_color (rgb_color (0, 255, 255));
      ren->fill (rs->item->x1, rs->item->y1, rs->item->x2, rs->item->y2);
      rs= rs->next;
    }
  }
}

void
edit_interface_rep::draw_cursor (renderer ren) {
  if (!temp_invalid_cursor && (got_focus || full_screen)) {
    cursor cu= get_cursor();
    if (!inside_active_graphics ()) {
      cu->y1 -= 2*pixel; cu->y2 += 2*pixel;
      SI x1= cu->ox + ((SI) (cu->y1 * cu->slope)), y1= cu->oy + cu->y1;
      SI x2= cu->ox + ((SI) (cu->y2 * cu->slope)), y2= cu->oy + cu->y2;
      ren->set_line_style (pixel);
      string mode= get_env_string (MODE);
      string family, series;
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
      if (cu->valid) {
	if (mode == "math")
	  ren->set_color (rgb_color (192, 0, 255));
	else ren->set_color (red);
      }
      else ren->set_color (green);
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
  ren->set_background (light_grey);
  string medium= get_init_string (PAGE_MEDIUM);
  if ((medium == "papyrus") || (medium == "paper"))
    ren->clear_pattern (max (eb->x2, r->x1), r->y1,
			r->x2, min (eb->y2+ 2*pixel, r->y2));
  else if (medium == "paper")
    ren->clear_pattern (r->x1, r->y1, r->x2, min (eb->y1, r->y2));
}

void
edit_interface_rep::draw_context (renderer ren, rectangle r) {
  int i;
  ren->set_color (light_grey);
  ren->set_line_style (pixel);
  for (i=1; i<N(eb[0]); i++) {
    SI y= eb->sy(0)+ eb[0]->sy2(i);
    if ((y >= r->y1) && (y < r->y2))
      ren->line (r->x1, y, r->x2, y);
  }
  draw_surround (ren, r);
}

void
edit_interface_rep::draw_selection (renderer ren) {
  if (!is_nil (locus_rects)) {
    rectangles rs= locus_rects;
    while (!is_nil (rs)) {
      ren->set_color (rgb_color (32, 160, 96));
      ren->fill (rs->item->x1, rs->item->y1, rs->item->x2, rs->item->y2);
      rs= rs->next;
    }
  }
  if (made_selection) {
    rectangles rs= selection_rects;
    while (!is_nil (rs)) {
      ren->set_color (table_selection? rgb_color (192, 0, 255): red);
      ren->fill (rs->item->x1, rs->item->y1, rs->item->x2, rs->item->y2);
      rs= rs->next;
    }
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
	  ren->set_line_style (pixel);
	  ren->set_color (red);
	  ren->line (cu->ox, cu->oy-5*pixel, cu->ox, cu->oy+5*pixel);
	  ren->line (cu->ox-5*pixel, cu->oy, cu->ox+5*pixel, cu->oy);
        }
	else if (tm_curs == "graphics-cross-arrows") {
	  static int s= 6*pixel, a= 2*pixel;
	  ren->set_line_style (pixel);
	  ren->set_color (red);
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
edit_interface_rep::draw_pre (renderer ren, rectangle r) {
  // draw surroundings
  tree bg= get_init_value (BG_COLOR);
  ren->set_background_pattern (bg);
  clear_pattern_rectangles (ren, rectangles (translate (r, ren->ox, ren->oy)));
  draw_surround (ren, r);

  // predraw cursor
  renderer win= get_renderer (this);
  draw_cursor (ren);
  rectangles l= copy_always;
  while (!is_nil (l)) {
    rectangle lr (l->item);
    win->put_shadow (ren, lr->x1, lr->y1, lr->x2, lr->y2);
    l= l->next;
  }
}

void
edit_interface_rep::draw_post (renderer ren, rectangle r) {
  renderer win= get_renderer (this);
  win->set_shrinking_factor (sfactor);
  ren->set_shrinking_factor (sfactor);
  draw_context (ren, r);
  draw_env (ren);
  draw_selection (ren);
  draw_graphics (ren);
  draw_cursor (ren); // the text cursor must be drawn over the graphical object
  ren->set_shrinking_factor (1);
  win->set_shrinking_factor (1);
}

void
edit_interface_rep::draw_with_shadow (rectangle r) {
  renderer win= get_renderer (this);
  rectangle sr= r / sfactor;
  win->new_shadow (shadow);
  win->get_shadow (shadow, sr->x1, sr->y1, sr->x2, sr->y2);
  renderer ren= shadow;

  rectangles l;
  win->set_shrinking_factor (sfactor);
  ren->set_shrinking_factor (sfactor);
  draw_pre (ren, r);
  draw_text (ren, l);
  ren->set_shrinking_factor (1);
  win->set_shrinking_factor (1);

  if (ren->interrupted ()) {
    ren->set_shrinking_factor (sfactor);
    l= l & rectangles (translate (r, ren->ox, ren->oy));
    simplify (l);
    copy_always= translate (copy_always, ren->ox, ren->oy);
    while (!is_nil (copy_always)) {
      l= rectangles (copy_always->item, l);
      copy_always= copy_always->next;
    }
    ren->set_shrinking_factor (1);

    draw_post (ren, r);
    while (!is_nil(l)) {
      SI x1= (l->item->x1)/sfactor - ren->ox - PIXEL;
      SI y1= (l->item->y1)/sfactor - ren->oy - PIXEL;
      SI x2= (l->item->x2)/sfactor - ren->ox + PIXEL;
      SI y2= (l->item->y2)/sfactor - ren->oy + PIXEL;
      ren->outer_round (x1, y1, x2, y2);
      win->put_shadow (ren, x1, y1, x2, y2);
      l= l->next;
    }
  }
}

void
edit_interface_rep::draw_with_stored (rectangle r) {
  renderer win= get_renderer (this);
  //cout << "Redraw " << (r/(sfactor*PIXEL)) << "\n";

  /* Verify whether the backing store is still valid */
  if (!is_nil (stored_rects)) {
    SI w1, h1, w2, h2;
    win   -> get_extents (w1, h1);
    stored -> get_extents (w2, h2);
    if (stored->ox != win->ox || stored->oy != win->oy ||
	w1 != w2 || h1 != h2) {
      // cout << "x"; cout.flush ();
      stored_rects= rectangles ();
    }
  }

  /* Either draw with backing store or regenerate */
  rectangle sr= r / sfactor;
  if (is_nil (rectangles (r) - stored_rects) && !is_nil (stored_rects)) {
    // cout << "*"; cout.flush ();
    win->new_shadow (shadow);
    win->get_shadow (shadow, sr->x1, sr->y1, sr->x2, sr->y2);
    shadow->put_shadow (stored, sr->x1, sr->y1, sr->x2, sr->y2);
    draw_post (shadow, r);
    win->put_shadow (shadow, sr->x1, sr->y1, sr->x2, sr->y2);
  }
  else {
    // cout << "."; cout.flush ();
    draw_with_shadow (r);
    if (!win->interrupted ()) {
      if (inside_active_graphics ()) {
	shadow->new_shadow (stored);
	shadow->get_shadow (stored, sr->x1, sr->y1, sr->x2, sr->y2);
	//stored_rects= /*stored_rects |*/ rectangles (r);
	stored_rects= simplify (rectangles (r, stored_rects));
	//cout << "Stored: " << stored_rects << "\n";
	//cout << "M"; cout.flush ();
      }
      draw_post (shadow, r);
      win->put_shadow (shadow, sr->x1, sr->y1, sr->x2, sr->y2);
    }
    else draw_post (win, r);
  }
}

/******************************************************************************
* event handlers
******************************************************************************/

void
edit_interface_rep::handle_clear (SI x1, SI y1, SI x2, SI y2) {
  renderer win= get_renderer (this);
  x1 *= sfactor; y1 *= sfactor; x2 *= sfactor; y2 *= sfactor;
  win->set_shrinking_factor (sfactor);
  tree bg= get_init_value (BG_COLOR);
  win->set_background_pattern (bg);
  win->clear_pattern (max (eb->x1, x1), max (eb->y1, y1),
		      min (eb->x2, x2), min (eb->y2, y2));
  draw_surround (win, rectangle (x1, y1, x2, y2));
  win->set_shrinking_factor (1);
}

void
edit_interface_rep::handle_repaint (SI x1, SI y1, SI x2, SI y2) {
  if (env_change != 0) {
    system_warning ("Invalid situation (" * as_string (env_change) * ")",
		    "(edit_interface_rep::handle_repaint)");
    return;
  }

  /*
  // In the past, we used the code below in order to hide the trace of
  // a moving cursor. This code is now incorrect, because the rectangle
  // (x1, y1)--(x2, y2) does not correspond to the repaint region clipping.
  // Nevertheless, the code seems no longer necessary. In case it would be,
  // it should be moved somewhere inside the internal repaint routines.
  SI extra= 3 * get_init_int (FONT_BASE_SIZE) * PIXEL / (2*sfactor);
  SI X1= (x1-extra) * sfactor, Y1= (y1-extra) * sfactor;
  SI X2= (x2+extra) * sfactor, Y2= (y2+extra) * sfactor;
  draw_with_stored (rectangle (X1, Y1, X2, Y2));
  */

  // cout << "Repainting\n";
  draw_with_stored (rectangle (x1, y1, x2, y2) * sfactor);
  if (last_change-last_update > 0)
    last_change = texmacs_time ();
  // cout << "Repainted\n";
}
