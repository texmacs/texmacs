
/******************************************************************************
* MODULE     : edit_interface.cpp
* DESCRIPTION: interface between the editor and the window manager
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license and comes WITHOUT
* ANY WARRANTY WHATSOEVER. See the file $TEXMACS_PATH/LICENSE for more details.
* If you don't have this file, write to the Free Software Foundation, Inc.,
* 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
******************************************************************************/

#include "Interface/edit_interface.hpp"
#include "file.hpp"
#include "convert.hpp"
#include "server.hpp"
#include "tm_buffer.hpp"
#include "Metafont/tex_files.hpp"

extern void (*env_next_prog)(void);
extern void selection_correct (tree t, path i1, path i2, path& o1, path& o2);

/*static*/ string
MODE_LANGUAGE (string mode) {
  if (mode == "text") return LANGUAGE;
  else if (mode == "math") return MATH_LANGUAGE;
  else if (mode == "prog") return PROG_LANGUAGE;
  else if (mode == "src") return LANGUAGE;
  cerr << "Mode = " << mode << "\n";
  fatal_error ("invalid mode", "MODE_LANGUAGE", "edit_interface.cpp");
  return LANGUAGE;
}

/******************************************************************************
* Main edit_interface routines
******************************************************************************/

edit_interface_rep::edit_interface_rep ():
  env_change (0),
  last_change (texmacs_time()), last_update (last_change-1),
  do_animate (false), next_animate (last_change-1),
  full_screen (false), got_focus (false),
  sh_s (""), sh_len (0),
  popup_win (NULL),
  sfactor (sv->get_default_shrinking_factor ()),
  pixel (sfactor*PIXEL), copy_always (),
  last_click (0), last_x (0), last_y (0), dragging (false),
  made_selection (false), table_selection (false),
  oc (0, 0), shadow (NULL), stored (NULL)
{
  input_mode= INPUT_NORMAL;
}

edit_interface_rep::~edit_interface_rep () {
  win->delete_shadow (shadow);
  win->delete_shadow (stored);
}

edit_interface_rep::operator tree () {
  return tuple ("editor", as_string (get_name ()));
}

void
edit_interface_rep::suspend () {
  got_focus= false;
  notify_change (THE_FOCUS);
  win->delete_shadow (shadow);
  win->delete_shadow (stored);
}

void
edit_interface_rep::resume () {
  got_focus= true;
  SERVER (menu_main ("(horizontal (link texmacs-menu))"));
  SERVER (menu_icons (0, "(horizontal (link texmacs-main-icons))"));
  SERVER (menu_icons (1, "(horizontal (link texmacs-context-icons))"));
  SERVER (menu_icons (2, "(horizontal (link texmacs-extra-icons))"));
  notify_change (THE_FOCUS + THE_EXTENTS);
}

display
edit_interface_rep::get_display () {
  return dis;
}

widget
edit_interface_rep::get_widget () {
  return widget (this);
}

/******************************************************************************
* Routines for dealing with shrinked coordinates
******************************************************************************/

int
edit_interface_rep::get_pixel_size () {
  return pixel;
}

void
edit_interface_rep::set_shrinking_factor (int sf) {
  if (sfactor != sf) {
    sfactor= sf;
    pixel  = sf*PIXEL;
    init_env (SFACTOR, as_string (sf));
    notify_change (THE_ENVIRONMENT);
    notify_change (THE_AUTOMATIC_SIZE);
  }
}

void
edit_interface_rep::invalidate (SI x1, SI y1, SI x2, SI y2) {
  this << emit_invalidate ((x1-sfactor+1)/sfactor, (y1-sfactor+1)/sfactor,
			   (x2+sfactor-1)/sfactor, (y2+sfactor-1)/sfactor);
}

void
edit_interface_rep::invalidate (rectangles rs) {
  while (!nil (rs)) {
    invalidate (rs->item->x1-pixel, rs->item->y1-pixel,
		rs->item->x2+pixel, rs->item->y2+pixel);
    rs= rs->next;
  }
}

void
edit_interface_rep::update_visible () {
  SERVER (get_visible (vx1, vy1, vx2, vy2));
  vx1 *= sfactor; vy1 *= sfactor; vx2 *= sfactor; vy2 *= sfactor;
}

SI
edit_interface_rep::get_window_height () {
  update_visible ();
  return vy2 - vy1;
}

void
edit_interface_rep::scroll_to (SI x, SI y) {
  stored_rects= rectangles ();
  SERVER (scroll_to (x/sfactor, y/sfactor));
}

void
edit_interface_rep::set_extents (SI x1, SI y1, SI x2, SI y2) {
  stored_rects= rectangles ();
  SERVER (set_extents ((x1-sfactor+1)/sfactor, (y1-sfactor+1)/sfactor,
		       (x2+sfactor-1)/sfactor, (y2+sfactor-1)/sfactor));
}

/******************************************************************************
* Scroll so as to make the cursor and the selection visible
******************************************************************************/

void
edit_interface_rep::cursor_visible () {
  cursor cu= get_cursor ();
  cu->y1 -= 2*pixel; cu->y2 += 2*pixel;
  update_visible ();
  if ((cu->ox+ ((SI) (cu->y1 * cu->slope)) <  vx1) ||
      (cu->ox+ ((SI) (cu->y2 * cu->slope)) >= vx2) ||
      (cu->oy+ cu->y1 <  vy1) ||
      (cu->oy+ cu->y2 >= vy2))
    {
      scroll_to (cu->ox- ((vx2-vx1)>>1), cu->oy+ ((vy2-vy1)>>1));
      this << emit_invalidate_all ();
    }
}

void
edit_interface_rep::selection_visible () {
  update_visible ();
  if ((vx2 - vx1 <= 80*pixel) || (vy2 - vy1 <= 80*pixel)) return;

  bool scroll_x=
    (end_x <  vx1 + 20*pixel) ||
    (end_x >= vx2 - 20*pixel);
  bool scroll_y=
    (end_y <  vy1 + 20*pixel) ||
    (end_y >= vy2 - 20*pixel);
  SI new_x= vx1;
  if (scroll_x) new_x= end_x - ((vx2-vx1)>>1);
  SI new_y= vy2;
  if (scroll_y) new_y= end_y + ((vy2-vy1)>>1);

  if (scroll_x || scroll_y) {
    scroll_to (new_x, new_y);
    this << emit_invalidate_all ();
    SI old_vx1= vx1, old_vy1= vy1;
    update_visible ();
    end_x += vx1- old_vx1;
    end_y += vy1- old_vy1;
  }
}

/******************************************************************************
* Computation of environment rectangles
******************************************************************************/

static bool
is_graphical (tree t) {
  return
    is_func (t, _POINT) ||
    is_func (t, LINE) || is_func (t, CLINE) ||
    is_func (t, ARC) || is_func (t, CARC) ||
    is_func (t, SPLINE) || is_func (t, CSPLINE);
}

void
edit_interface_rep::compute_env_rects (path p, rectangles& rs, bool recurse) {
  p= path_up (p);
  if (p == rp) return;
  tree st= subtree (et, p);
  if (is_atomic (st) ||
      drd->is_child_enforcing (st) ||
      //is_document (st) || is_concat (st) ||
      is_func (st, TABLE) || is_func (st, SUBTABLE) ||
      is_func (st, ROW) || is_func (st, TFORMAT) ||
      is_graphical (st) ||
      (is_func (st, WITH) && is_graphical (st[N(st)-1])) ||
      (is_compound (st, "math", 1) &&
       is_compound (subtree (et, path_up (p)), "input")))
    compute_env_rects (p, rs, recurse);
  else {
    bool right;
    path p1= p * 0, p2= p * 1, q1, q2;
    if (is_script (subtree (et, p), right)) {
      p1= start (et, p * 0);
      p2= end   (et, p * 0);
    }
    if (is_func (st, CELL)) { q1= p1; q2= p2; }
    else selection_correct (et, p1, p2, q1, q2);
    selection sel= eb->find_check_selection (q1, q2);
    rs << simplify (::correct (thicken (sel->rs, pixel, 3*pixel) -
			       thicken (sel->rs, 0, 2*pixel)));
    if (recurse) compute_env_rects (p, rs, recurse);
  }
}

/******************************************************************************
* handling changes
******************************************************************************/

void
edit_interface_rep::notify_change (int change) {
  env_change= env_change | change;
}

bool
edit_interface_rep::has_changed (int question) {
  return (env_change & question) != 0;
}

int
edit_interface_rep::idle_time (int event_type) {
  if (env_change == 0 &&
      win->repainted () &&
      (!dis->check_event (event_type)) &&
      got_focus)
    return texmacs_time () - last_change;
  else return 0;
}

int
edit_interface_rep::change_time () {
  return last_change;
}

void
edit_interface_rep::apply_changes () {
  //cout << "Apply changes\n";
  //cout << "et= " << et << "\n";
  //cout << "tp= " << tp << "\n";
  //cout << HRULE << "\n";
  if (env_change == 0) {
    if (last_change-last_update > 0 &&
	idle_time (INTERRUPTED_EVENT) >= 1000/6)
      {
	SERVER (menu_main ("(horizontal (link texmacs-menu))"));
	SERVER (menu_icons (0, "(horizontal (link texmacs-main-icons))"));
	SERVER (menu_icons (1, "(horizontal (link texmacs-context-icons))"));
	SERVER (menu_icons (2, "(horizontal (link texmacs-extra-icons))"));
	set_footer ();
	if (!win->interrupted ()) drd_update ();
	tex_autosave_cache ();
	last_update= last_change;
      }
    return;
  }

  // cout << "Applying changes " << env_change << " to " << get_name() << "\n";

  // cout << "Always\n";
  update_visible ();
  dis->partial_redrawing (nil (stored_rects));

  // cout << "Handling automatic resizing\n";
  if (env_change & THE_AUTOMATIC_SIZE) {
    if (get_init_string (PAGE_MEDIUM) == "automatic") {
      if (!attached())
	fatal_error ("Window not attached",
		     "edit_interface_rep::apply_changes");
      SI wx, wy;
      win->get_size (wx, wy);
      init_env (PAGE_SCREEN_WIDTH, as_string ((wx-20*PIXEL)*sfactor) * "tmpt");
      init_env (PAGE_SCREEN_HEIGHT, as_string (wy*sfactor) * "tmpt");
      notify_change (THE_ENVIRONMENT);
    }
  }

  // cout << "Handling selection\n";
  if (env_change & (THE_TREE+THE_ENVIRONMENT+THE_SELECTION)) {
    if (made_selection) invalidate (selection_rects);
  }

  // cout << "Handling environment\n";
  if (env_change & THE_ENVIRONMENT)
    typeset_invalidate_all ();

  // cout << "Handling tree\n";
  if (env_change & (THE_TREE+THE_ENVIRONMENT)) {
    typeset_invalidate_env ();
    if (input_mode == INPUT_NORMAL) {
      made_selection= false;
      set_selection (tp, tp);
    }
    selection_rects= rectangles ();
    SI x1, y1, x2, y2;
    typeset (x1, y1, x2, y2);
    invalidate (x1- 2*pixel, y1- 2*pixel, x2+ 2*pixel, y2+ 2*pixel);
    // check_data_integrety ();
    the_ghost_cursor()= eb->find_check_cursor (tp);
  }

  // cout << "Handling extents\n";
  if (env_change & (THE_TREE+THE_ENVIRONMENT+THE_EXTENTS))
    set_extents (eb->x1, eb->y1, eb->x2, eb->y2);

  // cout << "Cursor\n";
  if (env_change & (THE_TREE+THE_ENVIRONMENT+THE_EXTENTS+
		    THE_CURSOR+THE_FOCUS)) {
    SI /*P1= pixel,*/ P2= 2*pixel, P3= 3*pixel;
    int THE_CURSOR_BAK= env_change & THE_CURSOR;
    go_to_here ();
    env_change= (env_change & (~THE_CURSOR)) | THE_CURSOR_BAK;
    if (env_change & (THE_TREE+THE_ENVIRONMENT+THE_EXTENTS+THE_CURSOR))
      if (!inside_active_graphics ())
	cursor_visible ();

    cursor cu= get_cursor();
    rectangle ocr (oc->ox+ ((SI) (oc->y1*oc->slope))- P3, oc->oy+ oc->y1- P3,
		   oc->ox+ ((SI) (oc->y2*oc->slope))+ P2, oc->oy+ oc->y2+ P3);
    copy_always= rectangles (ocr, copy_always);
    invalidate (ocr->x1, ocr->y1, ocr->x2, ocr->y2);
    rectangle ncr (cu->ox+ ((SI) (cu->y1*cu->slope))- P3, cu->oy+ cu->y1- P3,
		   cu->ox+ ((SI) (cu->y2*cu->slope))+ P2, cu->oy+ cu->y2+ P3);
    invalidate (ncr->x1, ncr->y1, ncr->x2, ncr->y2);
    copy_always= rectangles (ncr, copy_always);
    oc= copy (cu);

    rectangles old_rects= env_rects;
    env_rects= rectangles ();
    compute_env_rects (path_up (tp), env_rects, true);
    if (env_rects != old_rects) {
      invalidate (old_rects);
      invalidate (env_rects);
    }
    else if (env_change & THE_FOCUS) invalidate (env_rects);
    invalidate_graphical_object ();
  }

  // cout << "Handling selection\n";
  if (env_change & THE_SELECTION) {
    made_selection= selection_active_any ();
    if (made_selection) {
      table_selection= selection_active_table ();
      selection sel; selection_get (sel);
      /*
      selection_rects=
	simplify (::correct (thicken (sel->rs, pixel, pixel) - sel->rs));
      */
      selection_rects= simplify (::correct (thicken (sel->rs, pixel, 3*pixel) -
					    thicken (sel->rs, 0, 2*pixel)));
      invalidate (selection_rects);
    }
  }

  // cout << "Handling backing store\n";
  if (!nil (stored_rects)) {
    if (env_change & (THE_TREE+THE_ENVIRONMENT+THE_SELECTION+THE_EXTENTS))
      stored_rects= rectangles ();
  }
  if (inside_active_graphics ()) {
    SI gx1, gy1, gx2, gy2;
    if (find_graphical_region (gx1, gy1, gx2, gy2)) {
      rectangle gr= rectangle (gx1, gy1, gx2, gy2);
      if (!nil (gr - stored_rects))
	invalidate (gx1, gy1, gx2, gy2);
    }
  }

  // cout << "Handling environment changes\n";
  if (env_change & THE_ENVIRONMENT)
    this << emit_invalidate_all ();

  // cout << "Applied changes\n";
  env_change  = 0;
  last_change = texmacs_time ();
  last_update = last_change-1;
}

/******************************************************************************
* Animations
******************************************************************************/

void
edit_interface_rep::animate () {
  // cout << do_animate << ", " << next_animate << "\n";
  if (do_animate && texmacs_time () - next_animate >= 0) {
    bool flag= false;
    time_t at= 0;
    rectangles rs;
    eb->anim_get_invalid (flag, at, rs);
    if (flag && texmacs_time () - at >= 0)
      invalidate (rs);
    do_animate  = flag;
    next_animate= at;
  }
}

/******************************************************************************
* Miscellaneous routines
******************************************************************************/

void
edit_interface_rep::full_screen_mode (bool flag) {
  full_screen= flag;
}

void
edit_interface_rep::before_menu_action () {
  buf->mark_undo_block ();
  set_input_normal ();
}

void
edit_interface_rep::after_menu_action () {
  notify_change (THE_DECORATIONS);
}

/******************************************************************************
* event handlers
******************************************************************************/

void
edit_interface_rep::handle_get_size (get_size_event ev) {
  dis->get_extents (ev->w, ev->h);
}

void
edit_interface_rep::handle_attach_window (attach_window_event ev) {
  basic_widget_rep::handle_attach_window (ev);
}

void
edit_interface_rep::handle_resize (resize_event ev) { (void) ev;
  if (get_init_string (PAGE_MEDIUM) == "automatic")
    notify_change (THE_AUTOMATIC_SIZE);
  notify_change (THE_TREE);
}

void
edit_interface_rep::handle_set_integer (set_integer_event ev) {
  if (ev->which == "shrinking factor")
    set_shrinking_factor (ev->i);
  else a[0]->a[0] << ev;
}
