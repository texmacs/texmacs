
/******************************************************************************
* MODULE     : edit_interface.cpp
* DESCRIPTION: interface between the editor and the window manager
* COPYRIGHT  : (C) 1999  Joris van der Hoeven
*******************************************************************************
* This software falls under the GNU general public license version 3 or later.
* It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
* in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
******************************************************************************/

#include "Interface/edit_interface.hpp"
#include "file.hpp"
#include "convert.hpp"
#include "server.hpp"
#include "tm_window.hpp"
#include "Metafont/tex_files.hpp"
#include "data_cache.hpp"
#include "drd_mode.hpp"
#include "message.hpp"
#include "tree_traverse.hpp"
#ifdef EXPERIMENTAL
#include "../../Style/Evaluate/evaluate_main.hpp"
#endif

extern void (*env_next_prog)(void);

/*static*/ string
MODE_LANGUAGE (string mode) {
  if (mode == "text") return LANGUAGE;
  else if (mode == "math") return MATH_LANGUAGE;
  else if (mode == "prog") return PROG_LANGUAGE;
  else if (mode == "src") return LANGUAGE;
  cerr << "Mode = " << mode << "\n";
  FAILED ("invalid mode");
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
  sh_s (""), sh_mark (0), pre_edit_s (""), pre_edit_mark (0),
  popup_win (),
  message_l (""), message_r (""), last_l (""), last_r (""),
  sfactor (sv->get_default_shrinking_factor ()),
  pixel (sfactor*PIXEL), copy_always (),
  last_click (0), last_x (0), last_y (0), dragging (false),
  made_selection (false), table_selection (false),
  oc (0, 0), temp_invalid_cursor (false),
  shadow (NULL), stored (NULL),
  cur_sb (2), cur_wb (2)
{
  input_mode= INPUT_NORMAL;
  gui_root_extents (cur_wx, cur_wy);
}

edit_interface_rep::~edit_interface_rep () {
  if (is_attached (this)) {
    renderer ren= get_renderer (this);
    ren->delete_shadow (shadow);
    ren->delete_shadow (stored);
  }
}

edit_interface_rep::operator tree () {
  return tuple ("editor", as_string (get_name ()));
}

void
edit_interface_rep::suspend () {
  if (got_focus) {
    interrupt_shortcut ();
    set_message ("", "", false);
  }
  got_focus= false;
  notify_change (THE_FOCUS);
  if (is_attached (this)) {
    renderer ren= get_renderer (this);
    ren->delete_shadow (shadow);
    ren->delete_shadow (stored);
  }
}

void
edit_interface_rep::resume () {
  got_focus= true;
  SERVER (menu_main ("(horizontal (link texmacs-menu))"));
  SERVER (menu_icons (0, "(horizontal (link texmacs-main-icons))"));
  SERVER (menu_icons (1, "(horizontal (link texmacs-mode-icons))"));
  SERVER (menu_icons (2, "(horizontal (link texmacs-focus-icons))"));
  SERVER (menu_icons (3, "(horizontal (link texmacs-extra-icons))"));
  cur_sb= 2;
  tp= make_cursor_accessible (tp, true);
  notify_change (THE_FOCUS + THE_EXTENTS + THE_CURSOR);
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
  }
}

void
edit_interface_rep::invalidate (SI x1, SI y1, SI x2, SI y2) {
  send_invalidate (this, (x1-sfactor+1)/sfactor, (y1-sfactor+1)/sfactor,
		         (x2+sfactor-1)/sfactor, (y2+sfactor-1)/sfactor);
}

void
edit_interface_rep::invalidate (rectangles rs) {
  while (!is_nil (rs)) {
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
  copy_always = rectangles ();
  SERVER (scroll_to (x/sfactor, y/sfactor));
}

void
edit_interface_rep::set_extents (SI x1, SI y1, SI x2, SI y2) {
  stored_rects= rectangles ();
  copy_always = rectangles ();
  SERVER (set_extents ((x1-sfactor+1)/sfactor, (y1-sfactor+1)/sfactor,
		       (x2+sfactor-1)/sfactor, (y2+sfactor-1)/sfactor));
}

/******************************************************************************
* Scroll so as to make the cursor and the selection visible
******************************************************************************/

void
edit_interface_rep::cursor_visible () {
  path sp= find_innermost_scroll (eb, tp);
  cursor cu= get_cursor ();
  if (is_nil (sp)) {
    update_visible ();
    cu->y1 -= 2*pixel; cu->y2 += 2*pixel;
    if ((cu->ox+ ((SI) (cu->y1 * cu->slope)) <  vx1) ||
	(cu->ox+ ((SI) (cu->y2 * cu->slope)) >= vx2) ||
	(cu->oy+ cu->y1 <  vy1) ||
	(cu->oy+ cu->y2 >= vy2))
      {
	scroll_to (cu->ox- ((vx2-vx1)>>1), cu->oy+ ((vy2-vy1)>>1));
	send_invalidate_all (this);
      }
  }
  else {
    SI x, y, sx, sy;
    rectangle outer, inner;
    find_canvas_info (eb, sp, x, y, sx, sy, outer, inner);
    if ((cu->ox+ ((SI) (cu->y1 * cu->slope)) < x + outer->x1) ||
	(cu->ox+ ((SI) (cu->y2 * cu->slope)) > x + outer->x2))
      {
	SI tx= inner->x2 - inner->x1;
	SI cx= outer->x2 - outer->x1;
	if (tx > cx) {
	  SI outer_cx= cu->ox - x;
	  SI inner_cx= outer_cx - sx;
	  SI dx= inner_cx - inner->x1;
	  double p= 100.0 * ((double) (dx - (cx>>1))) / ((double) (tx-cx));
	  p= max (min (p, 100.0), 0.0);
	  tree old_xt= eb[path_up (sp)]->get_info ("scroll-x");
	  tree new_xt= as_string (p) * "%";
	  if (new_xt != old_xt && is_accessible (obtain_ip (old_xt))) {
	    object fun= symbol_object ("tree-set");
	    object cmd= list_object (fun, old_xt, new_xt);
	    exec_delayed (scheme_cmd (cmd));
	    temp_invalid_cursor= true;
	  }
	}
      }
    if ((cu->oy+ cu->y1 < y + outer->y1) ||
	(cu->oy+ cu->y2 > y + outer->y2))
      {
	SI ty= inner->y2 - inner->y1;
	SI cy= outer->y2 - outer->y1;
	if (ty > cy) {
	  SI outer_cy= cu->oy + ((cu->y1 + cu->y2) >> 1) - y;
	  SI inner_cy= outer_cy - sy;
	  SI dy= inner_cy - inner->y1;
	  double p= 100.0 * ((double) (dy - (cy>>1))) / ((double) (ty-cy));
	  p= max (min (p, 100.0), 0.0);
	  tree old_yt= eb[path_up (sp)]->get_info ("scroll-y");
	  tree new_yt= as_string (p) * "%";
	  if (new_yt != old_yt && is_accessible (obtain_ip (old_yt))) {
	    object fun= symbol_object ("tree-set");
	    object cmd= list_object (fun, old_yt, new_yt);
	    exec_delayed (scheme_cmd (cmd));
	    temp_invalid_cursor= true;
	  }
	}
      }
  }
}

void
edit_interface_rep::selection_visible () {
  update_visible ();
  if ((vx2 - vx1 <= 80*pixel) || (vy2 - vy1 <= 80*pixel)) return;

  SI extra= (cur_sb == 1? 20 * pixel: 0);
  bool scroll_x= (end_x < vx1 + extra) || (end_x >= vx2 - extra);
  bool scroll_y= (end_y < vy1 + extra) || (end_y >= vy2 - extra);
  SI new_x= vx1;
  if (scroll_x) new_x= end_x - ((vx2-vx1)>>1);
  SI new_y= vy2;
  if (scroll_y) new_y= end_y + ((vy2-vy1)>>1);

  if (scroll_x || scroll_y) {
    scroll_to (new_x, new_y);
    send_invalidate_all (this);
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

static void
correct_adjacent (rectangles& rs1, rectangles& rs2) {
  if (N(rs1) != 1 || N(rs2) != 1) return;
  SI bot1= rs1->item->y1;
  SI top2= rs2->item->y2;
  if (rs1->item->y1 <= rs2->item->y1) {
    //cout << "Discard " << rs1->item->y1 << ", " << rs2->item->y1 << "\n";
    return;
  }
  if (rs1->item->y2 <= rs2->item->y2) {
    //cout << "Discard " << rs1->item->y2 << ", " << rs2->item->y2 << "\n";
    return;
  }
  SI mid= (bot1 + top2) >> 1;
  rs1->item->y1= mid;
  rs2->item->y2= mid;
}

void
edit_interface_rep::compute_env_rects (path p, rectangles& rs, bool recurse) {
  if (p == rp) return;
  tree st= subtree (et, p);
  if ((is_func (st, TABLE) || is_func (st, SUBTABLE)) &&
      recurse && get_preference ("show table cells") == "on") {
    rectangles rl;
    for (int i=0; i<N(st); i++) {
      if (is_func (st[i], ROW))
        for (int j=0; j<N(st[i]); j++) {
          selection sel= eb->find_check_selection (p*i*j*0, p*i*j*1);
          rectangles rsel= copy (thicken (sel->rs, 0, 2 * pixel));
          if (i > 0 && is_func (st[i-1], ROW) && j < N(st[i-1])) {
            selection bis= eb->find_check_selection (p*(i-1)*j*0, p*(i-1)*j*1);
            rectangles rbis= copy (thicken (bis->rs, 0, 2 * pixel));
            correct_adjacent (rbis, rsel);
          }
          if (i+1 < N(st) && is_func (st[i+1], ROW) && j < N(st[i+1])) {
            selection bis= eb->find_check_selection (p*(i+1)*j*0, p*(i+1)*j*1);
            rectangles rbis= copy (thicken (bis->rs, 0, 2 * pixel));
            correct_adjacent (rsel, rbis);
          }
          rectangles selp= thicken (rsel,  pixel/2,  pixel/2);
          rectangles selm= thicken (rsel, -pixel/2, -pixel/2);
          rl << simplify (::correct (selp - selm));
        }
    }
    rs << simplify (rl);
    if (recurse) compute_env_rects (path_up (p), rs, recurse);
  }
  else if (is_atomic (st) ||
           drd->is_child_enforcing (st) ||
           //is_document (st) || is_concat (st) ||
           is_func (st, TABLE) || is_func (st, SUBTABLE) ||
           is_func (st, ROW) || is_func (st, TFORMAT) ||
           is_graphical (st) ||
           (is_func (st, WITH) && is_graphical (st[N(st)-1])) ||
           (is_func (st, WITH) && is_func (st[N(st)-1], TEXT_AT)) ||
           (is_compound (st, "math", 1) &&
            is_compound (subtree (et, path_up (p)), "input")))
    compute_env_rects (path_up (p), rs, recurse);
  else {
    int new_mode= DRD_ACCESS_NORMAL;
    if (get_init_string (MODE) == "src") new_mode= DRD_ACCESS_SOURCE;
    int old_mode= set_access_mode (new_mode);
    tree st= subtree (et, p);
    if (is_accessible_cursor (et, p * right_index (st)) || in_source ()) {
      bool right;
      path p1= p * 0, p2= p * 1, q1, q2;
      if (is_script (subtree (et, p), right)) {
	p1= start (et, p * 0);
	p2= end   (et, p * 0);
      }
      if (is_func (st, CELL)) { q1= p1; q2= p2; }
      else selection_correct (p1, p2, q1, q2);
      selection sel= eb->find_check_selection (q1, q2);
      if (N(focus_get ()) >= N(p))
        if (!recurse || get_preference ("show full context") == "on")
          rs << outline (sel->rs, pixel);
    }
    set_access_mode (old_mode);
    if (recurse || N(rs) == 0)
      compute_env_rects (path_up (p), rs, recurse);
  }
}

/******************************************************************************
* handling changes
******************************************************************************/

void
edit_interface_rep::notify_change (int change) {
  env_change= env_change | change;
  needs_update ();
  if ((change & (THE_TREE | THE_SELECTION | THE_CURSOR)) != 0)
    manual_focus_set (path (), (change & THE_TREE) != 0);
}

bool
edit_interface_rep::has_changed (int question) {
  return (env_change & question) != 0;
}

int
edit_interface_rep::idle_time (int event_type) {
  if (env_change == 0 &&
      get_renderer (this) -> repainted () &&
      (!check_event (event_type)) &&
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
      SERVER (menu_icons (1, "(horizontal (link texmacs-mode-icons))"));
      SERVER (menu_icons (2, "(horizontal (link texmacs-focus-icons))"));
      SERVER (menu_icons (3, "(horizontal (link texmacs-extra-icons))"));
      set_footer ();
      if (!get_renderer (this) -> interrupted ()) drd_update ();
      cache_memorize ();
      last_update= last_change;
    }
    return;
  }
  
  // cout << "Applying changes " << env_change << " to " << get_name() << "\n";
  // time_t t1= texmacs_time ();
  
  // cout << "Always\n";
  update_visible ();
  
  // cout << "Handling automatic resizing\n";
  int sb= 1;
  if (is_attached (this) && get_init_string (PAGE_MEDIUM) == "automatic") {
    SI wx, wy;
    ::get_size (get_window (this), wx, wy);
    if (get_init_string (SCROLL_BARS) == "false") sb= 0;
    if (get_server () -> in_full_screen_mode ()) sb= 0;
#ifdef QTTEXMACS
    if (sb) wx -= 24 * PIXEL;
#else
    if (sb) wx -= 20 * PIXEL;
#endif
    if (wx != cur_wx || wy != cur_wy) {
      cur_wx= wx; cur_wy= wy;
      init_env (PAGE_SCREEN_WIDTH, as_string (wx*sfactor) * "tmpt");
      init_env (PAGE_SCREEN_HEIGHT, as_string (wy*sfactor) * "tmpt");
      notify_change (THE_ENVIRONMENT);
    }
  }
  if (sb != cur_sb) {
    cur_sb= sb;
    if (get_server() -> has_window()) {
      tm_window win= get_server () -> get_window ();
      win -> set_scrollbars (sb);
    }
  }
  
  // window decorations (menu bar, icon bars, footer)
  int wb= 2;
  if (is_attached (this)) {
    string val= get_init_string (WINDOW_BARS);
    if (val == "auto") wb= 2;
    else if (val == "false") wb= 0;
    else if (val == "true") wb= 1;
    if (wb != cur_wb) {
      cur_wb= wb;
      if (wb != 2) {
        get_server () -> show_header (wb);
        get_server () -> show_footer (wb);
      }
    }
  }
  
  // cout << "Handling selection\n";
  if (env_change & (THE_TREE+THE_ENVIRONMENT+THE_SELECTION)) {
    if (made_selection) {
      invalidate (selection_rects);
      if (!selection_active_any ()) {
        made_selection= false;
        set_selection (tp, tp);
        selection_rects= rectangles ();
      }
    }
  }
  
  // cout << "Handling environment\n";
  if (env_change & THE_ENVIRONMENT)
    typeset_invalidate_all ();

  // cout << "Handling tree\n";
  if (env_change & (THE_TREE+THE_ENVIRONMENT)) {
    typeset_invalidate_env ();
    SI x1, y1, x2, y2;
    typeset (x1, y1, x2, y2);
    invalidate (x1- 2*pixel, y1- 2*pixel, x2+ 2*pixel, y2+ 2*pixel);
    // check_data_integrety ();
    the_ghost_cursor()= eb->find_check_cursor (tp);
  }
  
#ifdef EXPERIMENTAL
  if (env_change & THE_ENVIRONMENT)
    environment_update ();
  if (env_change & THE_TREE) {
    cout << HRULE;
    mem= evaluate (ste, cct);
    tree rew= mem->get_tree ();
    cout << HRULE;
    cout << tree_to_texmacs (rew) << LF;
    //print_tree (rew);
  }
#endif
  
  // cout << "Handling extents\n";
  if (env_change & (THE_TREE+THE_ENVIRONMENT+THE_EXTENTS))
    set_extents (eb->x1, eb->y1, eb->x2, eb->y2);
  
  // cout << "Cursor\n";
  temp_invalid_cursor= false;
  if (env_change & (THE_TREE+THE_ENVIRONMENT+THE_EXTENTS+
                    THE_CURSOR+THE_SELECTION+THE_FOCUS)) {
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
   
    // set hot spot in the gui
    send_cursor (this, (cu->ox-sfactor+1)/sfactor, (cu->oy-sfactor+1)/sfactor);

    path sp= selection_get_cursor_path ();
    bool semantic_flag= semantic_active (path_up (sp));
    bool full_context= (get_preference ("show full context") == "on");
    bool table_cells= (get_preference ("show table cells") == "on");
    bool show_focus= (get_preference ("show focus") == "on");
    bool semantic_only= (get_preference ("show only semantic focus") == "on");
    rectangles old_env_rects= env_rects;
    rectangles old_foc_rects= foc_rects;
    env_rects= rectangles ();
    foc_rects= rectangles ();
    path pp= path_up (tp);
    tree pt= subtree (et, pp);
    if (none_accessible (pt));
    else pp= path_up (pp);
    if (full_context || table_cells)
      compute_env_rects (pp, env_rects, true);
    if (show_focus && (!semantic_flag || !semantic_only))
      compute_env_rects (pp, foc_rects, false);
    if (env_rects != old_env_rects) {
      invalidate (old_env_rects);
      invalidate (env_rects);
    }
    else if (env_change & THE_FOCUS) invalidate (env_rects);
    if (foc_rects != old_foc_rects) {
      invalidate (old_foc_rects);
      invalidate (foc_rects);
    }
    else if (env_change & THE_FOCUS) invalidate (foc_rects);
    
    rectangles old_sem_rects= sem_rects;
    bool old_sem_correct= sem_correct;
    sem_rects= rectangles ();
    sem_correct= true;
    if (semantic_flag && show_focus) {
      path sp= selection_get_cursor_path ();
      path p1= tp, p2= tp;
      if (selection_active_any ()) selection_get (p1, p2);
      sem_correct= semantic_select (path_up (sp), p1, p2, 2);
      if (!sem_correct) {
        path sr= semantic_root (path_up (sp));
        p1= start (et, sr);
        p2= end (et, sr);
      }
      path q1, q2;
      selection_correct (p1, p2, q1, q2);
      selection sel= eb->find_check_selection (q1, q2);
      sem_rects << outline (sel->rs, pixel);
    }
    if (sem_rects != old_sem_rects || sem_correct != old_sem_correct) {
      invalidate (old_sem_rects);
      invalidate (sem_rects);
    }
    else if (env_change & THE_FOCUS) invalidate (sem_rects);
    
    invalidate_graphical_object ();
  }
  
  // cout << "Handling selection\n";
  if (env_change & THE_SELECTION) {
    made_selection= selection_active_any ();
    if (made_selection) {
      table_selection= selection_active_table ();
      selection sel; selection_get (sel);
      rectangles rs= thicken (sel->rs, pixel, 3*pixel);
#ifndef QTTEXMACS
      rs= simplify (::correct (rs - thicken (rs, -pixel, -pixel)));
#endif
      selection_rects= rs;
      invalidate (selection_rects);
    }
  }
  
  // cout << "Handling locus highlighting\n";
  if (env_change & (THE_TREE+THE_ENVIRONMENT+THE_EXTENTS))
    update_active_loci ();
  if (env_change & THE_LOCUS) {
    if (locus_new_rects != locus_rects) {
      invalidate (locus_rects);
      invalidate (locus_new_rects);
      locus_rects= locus_new_rects;
    }
  }
  
  // cout << "Handling backing store\n";
  if (!is_nil (stored_rects)) {
    if (env_change & (THE_TREE+THE_ENVIRONMENT+THE_SELECTION+THE_EXTENTS))
      stored_rects= rectangles ();
  }
  if (inside_active_graphics ()) {
    SI gx1, gy1, gx2, gy2;
    if (find_graphical_region (gx1, gy1, gx2, gy2)) {
      rectangle gr= rectangle (gx1, gy1, gx2, gy2);
      if (!is_nil (gr - stored_rects))
        invalidate (gx1, gy1, gx2, gy2);
    }
  }
  
  // cout << "Handling environment changes\n";
  if (env_change & THE_ENVIRONMENT)
    send_invalidate_all (this);
  
  // cout << "Applied changes\n";
  // time_t t2= texmacs_time ();
  // if (t2 - t1 >= 10) cout << "apply_changes took " << t2-t1 << "ms\n";
  env_change  = 0;
  last_change = texmacs_time ();
  last_update = last_change-1;
  manual_focus_release ();
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
  send_invalidate_all (this);
}

void
edit_interface_rep::before_menu_action () {
  archive_state ();
  start_editing ();
  set_input_normal ();
}

void
edit_interface_rep::after_menu_action () {
  notify_change (THE_DECORATIONS);
  end_editing ();
}

/******************************************************************************
* event handlers
******************************************************************************/

void
edit_interface_rep::handle_get_size_hint (SI& w, SI& h) {
  gui_root_extents (w, h);
}

void
edit_interface_rep::handle_notify_resize (SI w, SI h) {
  (void) w; (void) h;
  notify_change (THE_TREE);
}

void
edit_interface_rep::handle_set_shrinking_factor (int sf) {
  set_shrinking_factor (sf);
}
