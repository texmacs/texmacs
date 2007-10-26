
/******************************************************************************
* MODULE     : edit_repaint.cpp
* DESCRIPTION: repaint invalid rectangles
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "Interface/edit_interface.hpp"
#include "window.hpp"

extern int nr_painted;
extern void clear_pattern_rectangles (ps_device dev, rectangles l);

/******************************************************************************
* repainting the window
******************************************************************************/

void
edit_interface_rep::draw_text (ps_device dev, rectangles& l) {
  nr_painted=0;
  bool tp_found= false;
  tree bg= get_init_value (BG_COLOR);
  dev->set_background_pattern (bg);
  refresh_needed= do_animate;
  refresh_next  = next_animate;
  eb->redraw (dev, eb->find_box_path (tp, tp_found), l);
  do_animate  = refresh_needed;
  next_animate= refresh_next;
}

void
edit_interface_rep::draw_env (ps_device dev) {
  if (!full_screen) {
    rectangles rs= env_rects;
    while (!nil (rs)) {
      dev->set_color (the_display->rgb (0, 255, 255));
      dev->fill (rs->item->x1, rs->item->y1, rs->item->x2, rs->item->y2);
      rs= rs->next;
    }
  }
}

void
edit_interface_rep::draw_cursor (ps_device dev) {
  if (!temp_invalid_cursor && (got_focus || full_screen)) {
    cursor cu= get_cursor();
    if (!inside_active_graphics ()) {
      cu->y1 -= 2*pixel; cu->y2 += 2*pixel;
      SI x1= cu->ox + ((SI) (cu->y1 * cu->slope)), y1= cu->oy + cu->y1;
      SI x2= cu->ox + ((SI) (cu->y2 * cu->slope)), y2= cu->oy + cu->y2;
      dev->set_line_style (pixel);
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
	  dev->set_color (the_display->rgb (192, 0, 255));
	else dev->set_color (the_display->red);
      }
      else dev->set_color (the_display->green);
      SI lserif= (series=="bold"? 2*pixel: pixel), rserif= pixel;
      if (family == "ss") lserif= rserif= 0;
      dev->line (x1-lserif, y1, x1+rserif, y1);
      if (y1<=y2-pixel) {
	dev->line (x1, y1, x2, y2-pixel);
	if (series == "bold") dev->line (x1-pixel, y1, x2-pixel, y2-pixel);
	dev->line (x2-lserif, y2-pixel, x2+rserif, y2-pixel);
      }
    }
  }
}

void
edit_interface_rep::draw_surround (ps_device dev, rectangle r) {
  dev->set_background (the_display->light_grey);
  string medium= get_init_string (PAGE_MEDIUM);
  if ((medium == "papyrus") || (medium == "paper"))
    dev->clear_pattern (max (eb->x2, r->x1), r->y1,
			r->x2, min (eb->y2+ 2*pixel, r->y2));
  else if (medium == "paper")
    dev->clear_pattern (r->x1, r->y1, r->x2, min (eb->y1, r->y2));
}

void
edit_interface_rep::draw_context (ps_device dev, rectangle r) {
  int i;
  dev->set_color (the_display->light_grey);
  dev->set_line_style (pixel);
  for (i=1; i<N(eb[0]); i++) {
    SI y= eb->sy(0)+ eb[0]->sy2(i);
    if ((y >= r->y1) && (y < r->y2))
      dev->line (r->x1, y, r->x2, y);
  }
  draw_surround (dev, r);
}

void
edit_interface_rep::draw_selection (ps_device dev) {
  if (!nil (locus_rects)) {
    rectangles rs= locus_rects;
    while (!nil (rs)) {
      dev->set_color (the_display->rgb (32, 160, 96));
      dev->fill (rs->item->x1, rs->item->y1, rs->item->x2, rs->item->y2);
      rs= rs->next;
    }
  }
  if (made_selection) {
    rectangles rs= selection_rects;
    while (!nil (rs)) {
      dev->set_color (table_selection? the_display->rgb (192, 0, 255):
		                       the_display->red);
      dev->fill (rs->item->x1, rs->item->y1, rs->item->x2, rs->item->y2);
      rs= rs->next;
    }
  }
}

void
edit_interface_rep::draw_graphics (ps_device dev) {
  if (got_focus || full_screen) {
    cursor cu= get_cursor();
    if (over_graphics(cu->ox, cu->oy) && inside_active_graphics ()) {
      eval ("(graphics-reset-context 'graphics-cursor)");
      draw_graphical_object (dev);
      string tm_curs= as_string (eval ("graphics-texmacs-pointer"));
      if (tm_curs != "none")
	if (tm_curs == "graphics-cross") {
	  dev->set_line_style (pixel);
	  dev->set_color (the_display->red);
	  dev->line (cu->ox, cu->oy-5*pixel, cu->ox, cu->oy+5*pixel);
	  dev->line (cu->ox-5*pixel, cu->oy, cu->ox+5*pixel, cu->oy);
        }
	else
	if (tm_curs == "graphics-cross-arrows") {
	  static int s= 6*pixel, a= 2*pixel;
	  dev->set_line_style (pixel);
	  dev->set_color (the_display->red);
	  dev->line (cu->ox, cu->oy-s, cu->ox, cu->oy+s);
	  dev->line (cu->ox-s, cu->oy, cu->ox+s, cu->oy);
	  dev->line (cu->ox, cu->oy-s,cu->ox-a, cu->oy-s+a);
	  dev->line (cu->ox, cu->oy-s, cu->ox+a, cu->oy-s+a);
	  dev->line (cu->ox, cu->oy+s, cu->ox-a, cu->oy+s-a);
	  dev->line (cu->ox, cu->oy+s, cu->ox+a, cu->oy+s-a);
	  dev->line (cu->ox-s, cu->oy, cu->ox-s+a, cu->oy+a);
	  dev->line (cu->ox-s, cu->oy, cu->ox-s+a, cu->oy-a);
	  dev->line (cu->ox+s, cu->oy, cu->ox+s-a, cu->oy+a);
	  dev->line (cu->ox+s, cu->oy, cu->ox+s-a, cu->oy-a);
        }
    }
    else eval ("(graphics-reset-context 'text-cursor)");
  }
}

void
edit_interface_rep::draw_pre (ps_device dev, rectangle r) {
  // draw surroundings
  tree bg= get_init_value (BG_COLOR);
  dev->set_background_pattern (bg);
  clear_pattern_rectangles (dev, rectangles (translate (r, dev->ox, dev->oy)));
  draw_surround (dev, r);

  // predraw cursor
  draw_cursor (dev);
  rectangles l= copy_always;
  while (!nil (l)) {
    rectangle lr (l->item);
    win->put_shadow (dev, lr->x1, lr->y1, lr->x2, lr->y2);
    l= l->next;
  }
}

void
edit_interface_rep::draw_post (ps_device dev, rectangle r) {
  win->set_shrinking_factor (sfactor);
  dev->set_shrinking_factor (sfactor);
  draw_context (dev, r);
  draw_env (dev);
  draw_selection (dev);
  draw_graphics (dev);
  draw_cursor (dev); // the text cursor must be drawn over the graphical object
  dev->set_shrinking_factor (1);
  win->set_shrinking_factor (1);
}

void
edit_interface_rep::draw_with_shadow (rectangle r) {
  rectangle sr= r / sfactor;
  win->new_shadow (shadow);
  win->get_shadow (shadow, sr->x1, sr->y1, sr->x2, sr->y2);
  ps_device dev= shadow;

  rectangles l;
  win->set_shrinking_factor (sfactor);
  dev->set_shrinking_factor (sfactor);
  draw_pre (dev, r);
  draw_text (dev, l);
  dev->set_shrinking_factor (1);
  win->set_shrinking_factor (1);

  if (dev->interrupted ()) {
    dev->set_shrinking_factor (sfactor);
    l= l & rectangles (translate (r, dev->ox, dev->oy));
    simplify (l);
    copy_always= translate (copy_always, dev->ox, dev->oy);
    while (!nil (copy_always)) {
      l= rectangles (copy_always->item, l);
      copy_always= copy_always->next;
    }
    dev->set_shrinking_factor (1);

    draw_post (dev, r);
    while (!nil(l)) {
      SI x1= (l->item->x1)/sfactor - dev->ox - PIXEL;
      SI y1= (l->item->y1)/sfactor - dev->oy - PIXEL;
      SI x2= (l->item->x2)/sfactor - dev->ox + PIXEL;
      SI y2= (l->item->y2)/sfactor - dev->oy + PIXEL;
      dev->outer_round (x1, y1, x2, y2);
      win->put_shadow (dev, x1, y1, x2, y2);
      l= l->next;
    }
  }
}

void
edit_interface_rep::draw_with_stored (rectangle r) {
  //cout << "Redraw " << (r/(sfactor*PIXEL)) << "\n";

  /* Verify whether the backing store is still valid */
  if (!nil (stored_rects)) {
    SI w1, h1, w2, h2;
    win    -> get_extents (w1, h1);
    stored -> get_extents (w2, h2);
    if (stored->ox!=win->ox || stored->oy!=win->oy || w1!=w2 || h1!=h2) {
      // cout << "x"; cout.flush ();
      stored_rects= rectangles ();
    }
  }

  /* Either draw with backing store or regenerate */
  rectangle sr= r / sfactor;
  if (nil (rectangles (r) - stored_rects) && !nil (stored_rects)) {
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
	win->new_shadow (stored);
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
edit_interface_rep::handle_clear (clear_event ev) {
  SI x1= ev->x1 * sfactor, y1= ev->y1 * sfactor;
  SI x2= ev->x2 * sfactor, y2= ev->y2 * sfactor;
  win->set_shrinking_factor (sfactor);
  tree bg= get_init_value (BG_COLOR);
  win->set_background_pattern (bg);
  win->clear_pattern (max (eb->x1, x1), max (eb->y1, y1),
		      min (eb->x2, x2), min (eb->y2, y2));
  draw_surround (win, rectangle (x1, y1, x2, y2));
  win->set_shrinking_factor (1);
}

void
edit_interface_rep::handle_repaint (repaint_event ev) {
  if (env_change != 0)
    system_warning ("Invalid situation (" * as_string (env_change) * ")",
		    "(edit_interface_rep::handle_repaint)");
  /*
  // In the past, we used the code below in order to hide the trace of
  // a moving cursor. This code is now incorrect, because the rectangle
  // (x1, y1)--(x2, y2) does not correspond to the repaint region clipping.
  // Nevertheless, the code seems no longer necessary. In case it would be,
  // it should be moved somewhere inside the internal repaint routines.
  SI extra= 3 * get_init_int (FONT_BASE_SIZE) * PIXEL / (2*sfactor);
  SI x1= (ev->x1-extra) * sfactor, y1= (ev->y1-extra) * sfactor;
  SI x2= (ev->x2+extra) * sfactor, y2= (ev->y2+extra) * sfactor;
  draw_with_stored (rectangle (x1, y1, x2, y2));
  */

  // cout << "Repainting\n";
  draw_with_stored (rectangle (ev->x1, ev->y1, ev->x2, ev->y2) * sfactor);
  if (win->interrupted ()) ev->stop= true;
  if (last_change-last_update > 0)
    last_change = texmacs_time ();
  // cout << "Repainted\n";
}
